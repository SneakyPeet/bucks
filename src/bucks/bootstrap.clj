(ns bucks.bootstrap
  (:require [bucks.ui.cli :as cli]
            [bucks.stores.filestore :as filestore]
            [bucks.core :as c])
  (:gen-class))

(def store-path "bucks.eventstore.edn")


(defn- create-dispatch-command
  []
  (fn [cmd]
    (c/handle-command {} cmd)))


(defn startup []
  (->> (create-dispatch-command)
       (cli/startup)))


(defn -main [& args]
  (startup))
