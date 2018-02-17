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

(def number-formatter (js/Intl.NumberFormat.))


(defn format-num [v] (.format number-formatter v))


(defn format-% [v] (str (format-num v) "%"))


(defn color-num [n] (cond (= 0 n) "has-text-light"
                          (> n 0) "has-text-primary"
                          :else "has-text-danger"))


(defn info-box [t info & [class]]
  [:div.has-text-centered.info-box
   [:p.heading t]
   [:p.title {:class (or class "has-text-light")} info]])


(defn col [size & children]
  [:div.column {:class (str "is-" size)}
   [:div.box.is-shadowless.has-text-grey-lighter
    children]])


(rum/defc chart < rum/static
  {:did-mount (fn [s]
                (let [[id f] (:rum/args s)]
                  (f id)
                  s))}
  [chart-id chart-renderer]
  [:div {:id chart-id}])


;;;; DATA


(defn wealth-guage [wi]
  (chart
   "wi-guage"
   (fn [id]
     (draw-guages
      id
      (data-table [["Level" "Value"]
                   ["Wealth Index" wi]])
      {:height 200 :majorTicks (range 0 6 1) :max 5 :min 0
       :greenColor "#00d1b2" :greenFrom 3 :greenTo 5
       :yellowColor "#ffde56" :yellowFrom 1.5 :yellowTo 3
       :redColor "#ff385f" :redFrom 0 :redTo 1.5}))))


(defn color-wi-num [n] (cond (> 1.5 n) "has-text-danger"
                             (> 3 n) "has-text-warning"
                             :else "has-text-success"))


(defn growth-chart [monthly-wi]
  (chart
   "growth-chart"
   (fn [id]
     (draw-area-chart
      id
      (->> monthly-wi
           (map (juxt :date :value))
           (into [["month" "value"]])
           data-table)
      {:title "GROWTH"}))))


(defn salaries-tooltip [{:keys [date value name]}]
  (str (format-num value) " " name " "  (.toLocaleDateString date "en-ZA")))


(defn salaries-chart [salaries]
  (chart
   "salaries-chart"
   (fn [id]
     (draw-area-chart
      id
      (->> salaries
           (map (juxt :date :value salaries-tooltip))
           (into [["month" "value" {:type "string" :role "tooltip"}]])
           data-table)
      {:title "SALARIES"}))))


(defn years [years]
  [:div.columns.is-multiline.is-centered
   (map-indexed
    (fn [i year]
      [:div.column.is-1.has-text-centered.asset-button
       {:key i :on-click #(show-modal :year year)}
       [:p.title.is-5 year]])
    years)])


(defmethod render-page :main [{:keys [data]}]
  (let [{:keys [wi asset-value growth-month growth-year]} (:current-values data)]
    [:div.columns.is-multiline.is-centered
     (col 3 (info-box "WEALTH INDEX" (format-num wi) (color-wi-num wi)))
     (col 3 (info-box "NET" (format-num asset-value)))
     (col 3 (info-box "MONTH TO DATE" (format-% growth-month) (color-num growth-month)))
     (col 3 (info-box "YEAR TO DATE" (format-% growth-year) (color-num growth-year)))
     (col 2 (wealth-guage wi))
     ;;todo asset groups
     (col 7 (growth-chart (:monthly-wi data)))
     ;;wi
     (col 7 (salaries-chart (:salaries data)))
     (col 12 (years (-> data :years keys)))
     ]))

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
