(ns bucks.bootstrap
  (:require [bucks.cli :as cli]
            [clojure.java.io :as io]
            [bucks.core :as c]
            [bucks.queries :as q]
            [clojure.string :as string]
            [clojure.java.browse :refer [browse-url]])
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


(defn init-report-index [store-path output]
  (-> (slurp "resources/public/index-base.html")
      (string/replace "\"{}\"" (pr-str (str (q/prep-report-data
                                             (read-string (slurp store-path))))))
      (#(spit output %))))


(def report-files ["css/style.css" "js/compiled/bucks.js"])
(def report-dir "report/")


(defn copy-report-file [path]
  (let [dest (str report-dir path)]
    (io/make-parents dest)
    (io/copy (io/as-file (str "resources/public/" path)) (io/file dest))))


(defn report [store-path]
  (let [index (str report-dir "index.html")]
    (doseq [f report-files] (copy-report-file f))
    (init-report-index store-path index)
    (browse-url index)))





(defn startup [store-path]
  (let [*state (init-store store-path)
        handle-command (fn [command]
                         (swap! *state #(apply-command % command)))
        query (fn [t o] (q/query t @*state o))
        gen-report (fn [_] (report store-path))]
    (cli/startup handle-command query gen-report)))


(defn -main [& args]
  (startup "bucks.store.edn"))

;;;; help

;(init-report-index "bucks.store.edn" "resources/public/index.html")
