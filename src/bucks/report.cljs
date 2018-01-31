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


(defn load-charts []
  (.load js/google.charts "current" (clj->js {:packages ["corechart" "annotationchart" "gauge"]}))
  (.setOnLoadCallback js/google.charts #(page :home)))

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


(defn draw-guages [id data opt]
  (draw-chart js/google.visualization.Gauge id data opt))


(defn draw-line-chart [id data opt]
  (draw-chart js/google.visualization.LineChart id data opt))


;;;; COMPONENTS

(defn wrap-args [f]
  (fn [s]
    (apply f (:rum/args s))
    s))

;; SALARY


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


(rum/defc salaries < rum/static
  {:did-mount (wrap-args salary-over-time)}
  [state]
  [:div {:id "salary-chart"}])


;; WI

(defn wealth-index-over-time [{:keys [wi wealth-index-goals]}]
  (let [headings (->> wealth-index-goals
                      vals
                      (map :name)
                      (concat ["Date" "WI"]))]
    (draw-line-chart
     "wi-chart"
     (->> wi
          (sort-by :timestamp)
          (map (fn [{:keys [timestamp goals]}]
                 (conj goals (js/Date. timestamp))))
          (into [headings])
          data-table)
     (chart-options {:height 250}))))


(rum/defc wi < rum/static
  {:did-mount (wrap-args wealth-index-over-time)}
  [state]
  [:div {:id "wi-chart"}])


(defn wealth-guage [{:keys [wi]}]
  (draw-guages
   "wi-guage"
   (data-table [["Lavel" "Value"]
                ["Wealth Index" (get (last wi) :wi 0)]])
   (chart-options {:height 200
                   :majorTicks (range 0 6 1)
                   :max 5
                   :min 0
                   :greenColor "green"
                   :greenFrom  3
                   :greenTo    5
                   :yellowColor "yellow"
                   :yellowFrom  1.5
                   :yellowTo    3
                   :redColor "red"
                   :redFrom  0
                   :redTo    1.5})))


(rum/defc wi-guage < rum/static
  {:did-mount (wrap-args wealth-guage)}
  [state]
  [:div {:id "wi-guage"}])


;;;; PAGES

(defmulti render-page (fn [state] (:page state)))


(defmethod render-page :default [state]
  [:strong "No renderer for page " (str (:page state))])


(defmethod render-page :loading [state]
  (load-charts)
  [:strong "loading"])


(defmethod render-page :home [state]
  [:div
   [:h1.title "Home"]
   (wi-guage state)
   (wi state)
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
