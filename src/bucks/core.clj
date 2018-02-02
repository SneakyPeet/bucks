(ns bucks.core
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [slingshot.slingshot :refer [throw+]]
            [clojure.string :as string]
            [bucks.queries :as queries]
            [java-time :as t])
  (:import [java.util Date]))

;;;; APP

(defmulti apply-command (fn [type state command] type))

(defn guard [spec data]
  (if-not (s/valid? spec data)
    (throw+ {:type :validation-error :error (expound/expound-str spec data)})
    data))


(defn throw-validation-error [error]
  (throw+ {:type :validation-error :error error}))


(defn throw-app-error [error]
  (throw+ {:type :app-error :error error}))


;;;; PREDICATES

(defn not-empty-string? [s] (and (string? s)
                                (not-empty s)))

(defn gt=zero? [n] (and (number? n) (not (neg? n))))


(defn gt-zero? [n] (and (number? n) (pos? n)))

(defn timestamp? [n] (and (integer? n) (pos? n)))


(defn year? [n] (boolean (and (number? n) (>= n 1900) (> 3000 n))))

(defn percentage? [n] (boolean (and (number? n) (>= n 0) (>= 100 n))))

;;;; SPECS

(s/def ::not-empty-string not-empty-string?)

(s/def ::source ::not-empty-string)

(s/def ::name ::not-empty-string)

(s/def ::timestamp timestamp?)

(s/def ::units gt=zero?)

(s/def ::age pos?)

(s/def ::amount gt=zero?)

(s/def ::value gt=zero?)

(s/def ::date-of-birth timestamp?)

(s/def ::note ::not-empty-string)

(s/def ::year year?)

(s/def ::percentage percentage?)


;; Salary

(s/def ::salary (s/keys :req-un [::timestamp ::value ::source]))

(s/def ::salaries (s/coll-of ::salary))


;; Transactions

(def transaction-types #{:deposit :withdrawal})

(s/def ::transaction-type #(contains? transaction-types %))

(s/def ::transaction (s/keys :req-un [::transaction-type ::timestamp ::amount]
                             :opt-un [::units ::note]))

(s/def ::transactions (s/coll-of ::transaction))


;; Assets

(def asset-types #{"TFSA" "RA" "Crypto" "Savings" "CFD" "ETF" "UnitTrust"})

(s/def ::asset-value (s/keys :req-un [::timestamp ::value]))

(s/def ::asset-values (s/coll-of ::asset-value))

(s/def ::asset-type ::not-empty-string)

(s/def ::exclude-from-net boolean?)

(s/def ::closed? boolean?)

(s/def ::asset (s/keys :req-un [::name
                                ::asset-type
                                ::transactions
                                ::asset-values
                                ::closed?]
                       :opt-un [::exclude-from-net]))

(s/def ::assets (s/map-of ::not-empty-string ::asset))


;; WealthIndex

(s/def ::wealth-index-goal (s/keys :req-un [::name ::age ::units]))

(s/def ::wealth-index-goals (s/map-of ::name ::wealth-index-goal))


;; Yearly Goal

(s/def ::yearly-goal (s/keys :req-un [::name ::year ::percentage]))

(s/def ::yearly-goals (s/coll-of ::yearly-goal))

;; State


(s/def ::state (s/keys :req-un [::salaries
                                ::assets
                                ::date-of-birth
                                ::wealth-index-goals
                                ::yearly-goals]))


;;;; CODE


(def initial-state {:salaries []
                    :assets {}
                    :date-of-birth 471146400000 ;;1984-12-06
                    :wealth-index-goals {}
                    :yearly-goals {}})


(defn guard-state [state] (guard ::state state))


(defn new-salary [s]
  (guard ::salary
         (select-keys s [:timestamp :value :source])))


(defn add-salary [state salary]
  (update state :salaries
          #(conj % (new-salary salary))))


(defn transaction [type t]
  (guard ::transaction
         (-> t
             (assoc :transaction-type type)
             (select-keys [:transaction-type :timestamp :amount :units :note]))))


(defn deposit [t] (transaction :deposit t))


(defn withdrawal [t] (transaction :withdrawal t))


(defn asset-value [v]
  (guard ::asset-value
         (select-keys v [:value :timestamp])))


(defn new-asset [asset]
  (guard ::asset
         (-> asset
             (select-keys [:name :asset-type :exclude-from-net])
             (assoc :closed? false
                    :transactions [(deposit asset)]
                    :asset-values [(asset-value (assoc asset :value (:amount asset)))]))))


(defn confirm-asset [{:keys [assets] :as state} {:keys [name] :as asset}]
  (when-not (contains? assets name)
    (throw-validation-error (str "Asset " name " does not exist")))
  (when (= true (get-in assets [name :closed?]))
    (throw-validation-error (str "Asset " name " has been closed"))))


(defn confirm-no-asset [{:keys [assets] :as state} {:keys [name] :as asset}]
  (when (contains? assets name)
    (throw-validation-error (str "Asset " name " already exist"))))


(defn add-asset [state asset]
  (confirm-no-asset state asset)
  (assoc-in state
            [:assets (:name asset)]
            (new-asset asset)))


(defn make-transaction [f state {:keys [name] :as transaction}]
  (confirm-asset state transaction)
  (-> state
      (update-in [:assets name :transactions]
                 #(conj % (f transaction)))
      (update-in [:assets name :asset-values]
                 #(conj % (asset-value transaction)))))


(defn make-deposit [state transaction] (make-transaction deposit state transaction))


(defn make-withdrawal [state transaction] (make-transaction withdrawal state transaction))


(defn set-asset-value [state {:keys [name] :as value}]
  (confirm-asset state value)
  (update-in state [:assets name :asset-values]
             #(conj % (asset-value value))))


(defn close-asset [state {:keys [name] :as close-transaction}]
  (let [asset (get-in state [:assets name])]
    (confirm-asset state asset)
    (-> state
        (make-withdrawal (assoc close-transaction :value 0))
        (assoc-in [:assets name :closed?] true))))


(defn set-date-of-birth [state {:keys [date-of-birth]}]
  (assoc state :date-of-birth (guard ::date-of-birth date-of-birth)))


(defn wealth-index-goal [g]
  (guard ::wealth-index-goal
         (select-keys g [:name :age :units])))


(defn add-wealth-index-goal [state {:keys [name] :as goal}]
  (assoc-in state [:wealth-index-goals name] (wealth-index-goal goal)))


(defn yearly-goal [g]
  (guard ::yearly-goal
         (select-keys g [:name :year :percentage])))


(defn add-yearly-goal [state {:keys [name year] :as goal}]
  (assoc-in state [:yearly-goals year name] (yearly-goal goal)))


;;;; IMPORTS

(defn as-timestamp-map [d]
  (->> (t/local-date "yyyy-MM-dd" d)
       (assoc {} :date)
       queries/unwrap-date))


(defn set-asset-values [state {:keys [name args]}]
  (when (odd? (count args)) (throw-validation-error "You should provide an odd amount of values"))
  (let [values
        (->> args
             (partition 2)
             (map (fn [[d v]]
                    (-> d
                        as-timestamp-map
                        (assoc :value (read-string v)
                               :name name))))
             )]
    (reduce (fn [r v]
              (set-asset-value r v))
            state
            values)))


;;;; COMMANDS

(defmethod apply-command :add-salary [_ & r] (apply add-salary r))
(defmethod apply-command :add-asset  [_ & r] (apply add-asset r))
(defmethod apply-command :close-asset  [_ & r] (apply close-asset r))
(defmethod apply-command :make-deposit [_ & r] (apply make-deposit r))
(defmethod apply-command :make-withdrawal [_ & r] (apply make-withdrawal r))
(defmethod apply-command :set-asset-value  [_ & r] (apply set-asset-value r))
(defmethod apply-command :set-asset-values [_ & r] (apply set-asset-values r))
(defmethod apply-command :set-date-of-birth [_ & r] (apply set-date-of-birth r))
(defmethod apply-command :add-wealth-index-goal [_ & r] (apply add-wealth-index-goal r))
(defmethod apply-command :add-yearly-goal [_ & r] (apply add-yearly-goal r))
