(ns bucks.report
  (:require [cljs.reader :as reader]
            [rum.core :as rum]))

(enable-console-print!)

(def initial-state {:page :home})


(defonce *state
  (atom
   (merge
    initial-state
    (reader/read-string (.. js/window -BUCKS)))))

(prn *state)

;;;; DEV


(defn on-js-reload []
   (swap! *state update-in [:__figwheel_counter] inc)
)
