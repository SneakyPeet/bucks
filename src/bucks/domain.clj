(ns bucks.domain
  (:require [bucks.utils :as u]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [slingshot.slingshot :refer [throw+]]))

;;;; SPEC

(defn guard [spec data]
  (when-not (s/valid? spec data)
    (throw+ [:validation-error (expound/expound-str spec data)])))


(s/def ::not-empty-string u/not-empty-string)

(s/def ::source ::not-empty-string)

(s/def ::date ::not-empty-string)


(defprotocol EventStore
  (load-all [this])
  (save [this events]))


(defmulti handle-command (fn [state command] (class command)))

;;;; Salaries

;; Spec

(s/def ::salary-amount u/gt-zero?)

(s/def ::salary (s/keys :req-un [::source
                                 ::date
                                 ::salary-amount]))

;; Commands

(defrecord ChangeSalary [source date salary-amount])

(defn change-salary-command [{:keys [source date salary-amount] :as c}]
  (guard ::salary c)
  (->ChangeSalary source date salary-amount))

;; Events

(defrecord Changedsalary [source date amount])

(defmethod handle-command ChangeSalary [_ {:keys [source date amount]}]
  (->ChangedSalary source date amount))







(def base-state
  {:salary []
   :assets {}})


(defn starting-state []
  (->> "config.edn"
       slurp
       read-string
       (merge base-state)))


(defmulti apply-event (fn [state event] (class event)))
