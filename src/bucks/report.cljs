(ns bucks.report
  (:require [cljs.reader :as reader]
            [rum.core :as rum]))

(enable-console-print!)

(def initial-state {:page :loading})


(defonce *state
  (atom
   (merge
    initial-state
    (reader/read-string (.. js/window -BUCKS)))))


(defn page [p] (swap! *state #(assoc % :page p)))


;;;; CHARTS

(defn data-table [coll]
  (prn coll)
  (.arrayToDataTable js/google.visualization (clj->js coll)))


(def chart-options clj->js)

(defn draw-chart [Chart id data opt]
  (.draw
   (Chart.
    (.getElementById js/document id))
   data opt))


(defn draw-annotation-chart [id data opt]
  (draw-chart js/google.visualization.AnnotationChart id data opt))


(defn salary-over-time [{:keys [salaries]}]
  (let [data
        (->> salaries
             (sort-by :timestamp)
             (map
              (fn [{:keys [timestamp value source]}]
                [(js/Date. timestamp) value source]))
             (into [["Date" "" "Source"]])
             data-table)
        opt (chart-options {:title "Salary"
                            :height 250})]
    (draw-annotation-chart "salary-chart" data opt)))

;;;; COMPONENTS

(defn wrap-args [f]
  (fn [s]
    (apply f (:rum/args s))
    s))


(rum/defc salaries < rum/static
  {:did-mount (wrap-args salary-over-time)}
  [state]
  [:div {:id "salary-chart"} "s"])

;;;; PAGES

(defmulti render-page (fn [state] (:page state)))


(defmethod render-page :default [state]
  [:strong "No renderer for page " (str (:page state))])


(defmethod render-page :loading [state]
  (.load js/google.charts "current" (clj->js {:packages ["corechart" "annotationchart"]}))
  (.setOnLoadCallback js/google.charts #(page :home))
  [:strong "loading"])


(defmethod render-page :home [state]
  [:div
   [:h1.title "Home"]
   (salaries state)])


;;;;APP

(rum/defc app < rum/reactive [state]
  (let [current-state (rum/react state)]
    (render-page current-state)))


(rum/mount (app *state) (.getElementById js/document "app"))

;;;; DEV


(defn on-js-reload []
   (swap! *state update-in [:__figwheel_counter] inc)
)
