(ns bucks.domain
  (:require [bucks.core :refer :all]
            [bucks.utils :as u]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [slingshot.slingshot :refer [throw+]]))


(s/def ::not-empty-string u/not-empty-string)

(s/def ::source ::not-empty-string)

(s/def ::date ::not-empty-string)


;;;; SALARIES

;; Spec

(s/def ::salary-amount u/gt-zero?)

(s/def :d/salary (s/keys :req-un [::source
                                  ::date
                                  ::salary-amount]))

;; Events

(defrecord ChangedSalary [source date salary-amount])

;; Commands

(defrecord ChangeSalary [source date salary-amount])

(defmethod handle-command ChangeSalary [_ {:keys [source date salary-amount]}]
  (->ChangedSalary source date salary-amount))







(def base-state
  {:salary []
   :assets {}})


(defn starting-state []
  (->> "config.edn"
       slurp
       read-string
       (merge base-state)))


(defmulti apply-event (fn [state event] (class event)))
