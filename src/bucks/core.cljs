(ns bucks.core
  (:require [bucks.domain :as domain]
            [bucks.example :as example]
            [bucks.parse :as parse]
            [rum.core :as rum]))

(enable-console-print!)

;;;; STATE

(def initial-data (->> example/test-piped-csv
                       (parse/parse parse/pipe)
                       parse/as-domain-values
                       domain/all-your-bucks))

(defonce *state (atom {:page :loading
                       :modal {:key :hidden :data nil}
                       :data initial-data}))


(defmulti render-page :page)


(defn page [page] (swap! *state #(assoc % :page page)))


(defmulti render-modal (fn [state] (get-in state [:modal :key])))


(defn show-modal [key data] (swap! *state #(assoc % :modal {:key key :data data})))


(defn hide-modal [] (swap! *state #(assoc % :modal {:key :hidden})))


;;;; CHARTS

(defn load-charts [callback]
  (.load js/google.charts "current" (clj->js {:packages ["corechart" "annotationchart" "gauge"]}))
  (.setOnLoadCallback js/google.charts callback))


(defn data-table [coll]
  (.arrayToDataTable js/google.visualization (clj->js coll)))


(def chart-options clj->js)

(def chart-base-color "#dbdbdb")

(def explorer-opt {:explorer {:actions ["dragToZoom" "rightClickToReset"]
                              :axis "horizontal"
                              :keepInBounds true}})

(defn draw-chart [Chart id data opt]
  (let [opt (merge
             {:animation {:startup true :duration 2000 :easing "out"}
              :colors ["#00d1b2" "#ff385f" "#ffde56" "#3372dd"]
              :backgroundColor "none"
              :legend {:textStyle {:color chart-base-color}
                       :position "bottom"}
              :titleTextStyle {:color chart-base-color}
              :hAxis {:gridlines {:color "none"}
                      :textStyle {:color chart-base-color}}
              :vAxis {:gridlines {:color chart-base-color}
                      :textStyle {:color chart-base-color}}
              :height 250}
             opt)
        chart (Chart. (.getElementById js/document id))]
    (.draw chart data (chart-options opt))))


(defn draw-annotation-chart [id data opt]
  (draw-chart js/google.visualization.AnnotationChart id data opt))


(defn draw-guages [id data opt]
  (draw-chart js/google.visualization.Gauge id data opt))


(defn draw-line-chart [id data opt]
  (draw-chart js/google.visualization.LineChart
              id data
              (merge explorer-opt opt)))


(defn draw-area-chart [id data opt]
  (draw-chart js/google.visualization.AreaChart
              id data
              (merge explorer-opt {:areaOpacity 0.1} opt)))


(defn draw-pie-chart [id data opt]
  (draw-chart js/google.visualization.PieChart
              id data
              (merge {:pieHole 0.4} opt)))


(defn draw-column-chart [id data opt]
  (draw-chart js/google.visualization.ColumnChart id data opt))


;;;; COMPONENTS


;;;; DATA

(defmethod render-page :main [state]
  "Hi")

;;;;APP

(rum/defc app < rum/reactive [state]
  (let [current-state (rum/react state)]
    [:div
     (when-not (= :hidden (get-in current-state [:modal :key])) (render-modal state))
     (render-page current-state)]))


(defmethod render-page :loading [state]
  [:div
   [:h1.title.has-text-primary.has-text-centered "Loading all the Things"]
   [:h2.subtitle.has-text-primary.has-text-centered "(╯°□°）╯︵ ┻━┻"]])


(rum/mount (app *state) (.getElementById js/document "app"))


(when (= :loading (:page @*state))
  (load-charts #(page :main)))

;;;;DEV


(defn on-js-reload []
  (swap! *state update-in [:__figwheel_counter] inc))
