(ns bucks.report
  (:require [cljs.reader :as reader]
            [rum.core :as rum]
            [goog.string :as gstring]
            [goog.string.format]))

(enable-console-print!)

(def initial-state {:page :loading})


(defonce *state
  (atom
   (merge
    initial-state
    (reader/read-string (.. js/window -BUCKS)))))


(defn page [p] (swap! *state #(assoc % :page p)))


(def number-formatter (js/Intl.NumberFormat.))

(defn s-number [v] (.format number-formatter v))

;;;; CHARTS


(defn load-charts []
  (.load js/google.charts "current" (clj->js {:packages ["corechart" "annotationchart" "gauge"]}))
  (.setOnLoadCallback js/google.charts #(page :home)))

(defn data-table [coll]
  (.arrayToDataTable js/google.visualization (clj->js coll)))


(def chart-options clj->js)

(def chart-base-color "#dbdbdb")

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
                      :textStyle {:color chart-base-color}}}
             opt)]
    (.draw
     (Chart.
      (.getElementById js/document id))
     data (chart-options opt))))


(defn draw-annotation-chart [id data opt]
  (draw-chart js/google.visualization.AnnotationChart id data opt))


(defn draw-guages [id data opt]
  (draw-chart js/google.visualization.Gauge id data opt))


(defn draw-line-chart [id data opt]
  (draw-chart js/google.visualization.LineChart
              id data
              (merge {:explorer {:actions ["dragToZoom" "rightClickToReset"]
                                 :axis "horizontal"
                                 :keepInBounds true}} opt)))


(defn draw-column-chart [id data opt]
  (draw-chart js/google.visualization.ColumnChart id data opt))


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
             (into [["date" "value" {:type "string" :role "annotation"}]])
             data-table)
        opt {:title "SALARY"
             :height 250
             :annotations {"Source" {:style "line"}}}]
    (draw-line-chart "salary-chart" data opt)))


(rum/defc salaries < rum/static
  {:did-mount (wrap-args salary-over-time)}
  [state]
  [:div {:id "salary-chart"}])


;; WI

(defn wealth-index-over-time [{:keys [wi wealth-index-goals]}]
  (let [headings (->> wealth-index-goals
                      vals
                      (map :name)
                      (concat ["date" "wealth index"]))]
    (draw-line-chart
     "wi-chart"
     (->> wi
          (sort-by :timestamp)
          (map (fn [{:keys [timestamp goals]}]
                 (conj goals (js/Date. timestamp))))
          (into [headings])
          data-table)
     {:title  "WEALTH INDEX"
      :height 250})))


(rum/defc wi < rum/static
  {:did-mount (wrap-args wealth-index-over-time)}
  [state]
  [:div {:id "wi-chart"}])


(defn wealth-guage [{:keys [wi]}]
  (draw-guages
   "wi-guage"
   (data-table [["Level" "Value"]
                ["Wealth Index" (get (last wi) :wi 0)]])
   {:height      200
    :majorTicks  (range 0 6 1)
    :max         5
    :min         0
    :greenColor  "green"
    :greenFrom   3
    :greenTo     5
    :yellowColor "yellow"
    :yellowFrom  1.5
    :yellowTo    3
    :redColor    "red"
    :redFrom     0
    :redTo       1.5}))


(rum/defc wi-guage < rum/static
  {:did-mount (wrap-args wealth-guage)}
  [state]
  [:div {:id "wi-guage"}])


;; NET

(defn net-bar [{:keys [wi]}]
  (draw-column-chart
   "net-bar"
   (->> wi
        (take-last 5)
        (map (juxt #(js/Date. (:timestamp %)) :net))
        (into [["month" "value"]])
        data-table)
   {:title "GROWTH"
    :bars "vertical"}))

(rum/defc net < rum/static
  {:did-mount (wrap-args net-bar)}
  [state]
  [:div {:id "net-bar"}])


;;;; PAGES

(defmulti render-page (fn [state] (:page state)))


(defmethod render-page :default [state]
  [:strong "No renderer for page " (str (:page state))])


(defmethod render-page :loading [state]
  (load-charts)
  [:strong "loading"])


(defn col [size & children]
  [:div.column {:class (str "is-" size)}
   [:div.box.is-shadowless.has-text-grey-lighter
    children]])


(defn info-box [t info]
  [:div.has-text-centered
   [:p.heading t]
   [:p.title.has-text-primary info]])


(defmethod render-page :home [{:keys [current] :as state}]
  [:div
   [:div.columns.is-multiline.is-centered
    (col 3 (info-box "WEALTH INDEX" (s-number (:wi current))))
    (col 3 (info-box "NET" (s-number (:net current))))
    (col 3 (info-box "MONTH TO DATE" (str (s-number (:monthly-growth current)) "%")))
    (col 3 (info-box "YEAR TO DATE" (str (s-number (:yearly-growth current)) "%")))
    (col 2 (wi-guage state))
    (col 6 (wi state))
    (col 4 (net state))
    (col 6 (salaries state))]])


;;;;APP

(rum/defc app < rum/reactive [state]
  (let [current-state (rum/react state)]
    (render-page current-state)))


(rum/mount (app *state) (.getElementById js/document "app"))

;;;; DEV


(defn on-js-reload []
   (swap! *state update-in [:__figwheel_counter] inc)
)
