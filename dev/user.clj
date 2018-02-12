(ns user
  (:require
   [figwheel-sidecar.repl-api :as f]
   [bucks.bootstrap :as bootstrap]
   [bucks.core :as bucks]
   [java-time.repl :as jt]))

;; user is a namespace that the Clojure runtime looks for and
;; loads if its available

;; You can place helper functions in here. This is great for starting
;; and stopping your webserver and other development services

;; The definitions in here will be available if you run "lein repl" or launch a
;; Clojure repl some other way

;; You have to ensure that the libraries you :require are listed in your dependencies

;; Once you start down this path
;; you will probably want to look at
;; tools.namespace https://github.com/clojure/tools.namespace
;; and Component https://github.com/stuartsierra/component


(defn fig-start
  "This starts the figwheel server and watch based auto-compiler."
  []
  ;; this call will only work are long as your :cljsbuild and
  ;; :figwheel configurations are at the top level of your project.clj
  ;; and are not spread across different lein profiles

  ;; otherwise you can pass a configuration into start-figwheel! manually
  (f/start-figwheel!))

(defn fig-stop
  "Stop the figwheel server and watch based auto-compiler."
  []
  (f/stop-figwheel!))

;; if you are in an nREPL environment you will need to make sure you
;; have setup piggieback for this to work
(defn cljs-repl
  "Launch a ClojureScript REPL that is connected to your build and host environment."
  []
  (f/cljs-repl))

(def test-file "bucks.store.edn")

(defn load-test-data
  "Hidrates Figwheel with test data from bucks.store.edn"
  []
  (bootstrap/init-report-index test-file "resources/public/index.html"))

(defn reset-test-data
  []
  (spit test-file (pr-str bucks/initial-state)))

;(reset-test-data)
;(load-test-data) 
