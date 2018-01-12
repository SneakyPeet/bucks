(ns bucks.core
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [slingshot.slingshot :refer [throw+]]))


;;;; EVENTS

(defprotocol EventStore
  (load-all [this])
  (save [this events]))


(defmulti handle-command (fn [state command] (class command)))


;;;; SPEC

(defn guard [spec data]
  (when-not (s/valid? spec data)
    (throw+ [:validation-error (expound/expound-str spec data)])))


(defn throw-app-error [error]
  (throw+ [:app-error error]))
