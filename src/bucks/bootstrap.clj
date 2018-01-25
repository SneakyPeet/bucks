(ns bucks.bootstrap
  (:require [bucks.cli :as cli]
            [clojure.java.io :as io]
            [bucks.core :as c])
  (:gen-class))


(defn- init-store [path]
  (io/make-parents path)
  (let [*store (if (.exists (io/file path))
                 (-> path slurp read-string atom)
                 (atom c/initial-state))]
    (add-watch *store :save-store
               (fn [_ _ _ new-state]
                 (spit path (pr-str new-state))))
    *store))


(defn- apply-command
  [state {:keys [type] :as command}]
  (c/apply-command type state (dissoc command :type)))


(defn startup [store-path]
  (let [*state (init-store store-path)
        handle-command (fn [command]
                           (swap! *state #(apply-command % command)))]
    (cli/startup handle-command)))


(defn -main [& args]
  (startup "bucks.store.edn"))
