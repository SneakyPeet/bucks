(ns bucks.core
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [slingshot.slingshot :refer [throw+]]
            [clojure.string :as string])
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

(defn gt-zero? [n] (and (number? n) (pos? n)))


(defn timestamp? [n] (and (integer? n) (pos? n)))


(defn year? [n] (boolean (and (number? n) (>= n 1900) (> 3000 n))))

(defn percentage? [n] (boolean (and (number? n) (>= n 0) (>= 100 n))))

;;;; SPECS

(s/def ::not-empty-string not-empty-string?)

(s/def ::source ::not-empty-string)

(s/def ::name ::not-empty-string)

(s/def ::timestamp timestamp?)

(s/def ::units gt-zero?)

(s/def ::age gt-zero?)

(s/def ::date-of-birth timestamp?)

(s/def ::note ::not-empty-string)

(s/def ::year year?)

;; Salary

(s/def ::salary (s/keys :req-un [::timestamp ::amount ::source]))

(s/def ::salaries (s/coll-of ::salary))


;; Transactions

(def transaction-types #{:deposit :withdrawal})

(s/def ::transaction-type #(contains? transaction-types %))

(s/def ::transaction (s/keys :req-un [::transaction-type ::timestamp ::amount]
                             :opt-un [::units ::note]))

(s/def ::transactions (s/coll-of ::transaction))


;; Assets

(def asset-types #{"TFSA" "RA" "Crypto" "Savings" "Shares" "UnitTrust"})

(s/def ::asset-value (s/keys :req-un [::date ::amount]))

(s/def ::asset-values (s/coll-of ::asset-values))

(s/def ::asset-type ::not-empty-string)

(s/def ::exclude-from-net? boolean?)

(s/def ::asset (s/keys :req-un [::name
                                ::asset-type
                                ::transactions
                                ::asset-values]
                       :opt-un [::exclude-from-net]))

(s/def ::assets (s/map-of ::not-empty-string ::asset))


;; WealthIndex

(s/def ::wealth-index-goal (s/keys :req-un [::name ::age ::units]))

(s/def ::wealth-index-goals (s/map-of ::name ::wealth-index-goal))


;; Yearly Goal

(s/def ::yearly-goal (s/keys :req-un [::name ::year ::percentage]))

(s/def ::yearly-goals (s/coll-of ::yearly-goal))

;; State

(s/def ::closed-assets ::assets)

(s/def ::state (s/keys :req-un [::salaries
                                ::assets
                                ::closed-assets
                                ::date-of-birth
                                ::wealth-index-goals
                                ::yearly-goals]))


;;;; CODE


(def initial-state {:salaries []
                    :assets {}
                    :closed-assets {}
                    :date-of-birth (.getTime (Date. 1984 12 6))
                    :wealth-index-goals {}
                    :yearly-goals []})


(defn guard-state [state] (guard ::state state))


(defn add-salary [state salary]
  (update state :salaries
          #(conj % (guard ::salary salary))))


(defn new-asset [asset]
  (-> asset
      (select-keys [:name :asset-type :exclude-from-net])
      (assoc :transactions []
             :asset-values [])))


(defn confirm-asset [{:keys [assets] :as state} {:keys [name] :as asset}]
  (when-not (contains? assets name)
    (throw-validation-error (str "Asset " name " does not exist"))))


(defn confirm-no-asset [{:keys [assets] :as state} {:keys [name] :as asset}]
  (when (contains? assets name)
    (throw-validation-error (str "Asset " name " already exist"))))


(defn add-asset [state asset]
  (confirm-no-asset state asset)
  (assoc-in state
            [:assets (:name asset)]
            (guard ::asset (new-asset asset))))


(defn close-asset [state {:keys [name]}]
  (let [asset (get-in state [:assets name])]
    (confirm-asset state asset)
    (-> state
        (assoc-in [:closed-assets name] asset)
        (update :assets #(dissoc % name)))))


(defn transaction [type t]
  (-> t
      (assoc :transaction-type type)
      (select-keys [:transaction-type :timestamp :amount :units :note])))


(defn deposit [t] (transaction :deposit t))


(defn withdrawal [t] (transaction :withdrawal t))


(defn make-transaction [f state {:keys [name] :as transaction}]
  (confirm-asset state transaction)
  (update-in state [:assets name :transactions]
             #(conj % (guard ::transaction (f transaction)))))


(defn make-deposit [state transaction] (make-transaction deposit state transaction))


(defn make-withdrawal [state transaction] (make-transaction withdrawal state transaction))


(defn set-date-of-birth [state dob]
  (assoc state :date-of-birth (guard ::date-of-birth dob)))


(defn add-wealth-index-goal [state {:keys [name] :as goal}]
  (assoc-in state [:wealth-index-goals name] (guard ::wealth-index-goal goal)))


(defn add-yearly-goal [state yearly-goal]
  (update state :yearly-goals
          #(conj % (guard ::yearly-goals yearly-goal))))


;;;; COMMANDS

(defmethod apply-command :add-salary [_ & r] (apply add-salary r))
(defmethod apply-command :add-asset  [_ & r] (apply add-asset r))
(defmethod apply-command :close-asset  [_ & r] (apply close-asset r))
(defmethod apply-command :make-deposit [_ & r] (apply make-deposit r))
(defmethod apply-command :make-withdrawal [_ & r] (apply make-withdrawal r))
(defmethod apply-command :set-date-of-birth [_ & r] (apply set-date-of-birth r))
(defmethod apply-command :add-wealth-index-goal [_ & r] (apply add-wealth-index-goal r))
(defmethod apply-command :add-yearly-goal [_ & r] (apply add-yearly-goal r))
