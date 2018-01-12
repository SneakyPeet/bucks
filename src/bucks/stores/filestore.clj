(ns bucks.stores.filestore
  (:require [bucks.core :refer :all]
            [clojure.java.io :as io]))


(defn- init-store [path]
  (io/make-parents path)
  (let [*store (if (.exists (io/file path))
                 (-> path slurp read-string atom)
                 (atom []))]
    (add-watch *store :save-store
               (fn [_ _ _ new-state]
                 (spit path (pr-str new-state))))
    *store))


(defn file-store [path]
  (let [*store (init-store path)]
    (reify
      EventStore
      (load-all [this] @*store)
      (save [this events]
        (cond
          (map? events) (swap! *store #(concat % [events]))
          (or (list? events) (vector? events)) (swap! *store #(concat % events))
          :else (throw-app-error "Events Needs to be a vec or a map"))))))
