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


(defmulti render-page (fn [state] (:page state)))


(defn page [p] (swap! *state #(assoc % :page p)))


(defmulti render-modal (fn [state] (:modal state)))


(defmethod render-modal :default [_] [:span])


(defn hide-modal [] (swap! *state #(dissoc % :modal)))


(defn show-asset [n]
  (swap! *state #(assoc %
                        :modal :asset
                        :asset n)))


(def number-formatter (js/Intl.NumberFormat.))


(defn s-number [v] (.format number-formatter v))


(defn date [m] (js/Date. (:timestamp m)))


(defn info-box [t info]
  [:div.has-text-centered
   [:p.heading t]
   [:p.title.has-text-primary info]])


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
                      :textStyle {:color chart-base-color}}
              :height 250}
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
                      (map (fn [{:keys [age units]}] (str units "@" age)))
                      (concat ["date" "actual"]))]
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
        (map (juxt date :net))
        (into [["month" "value"]])
        data-table)
   {:title "GROWTH"
    :bars "vertical"}))

(rum/defc net < rum/static
  {:did-mount (wrap-args net-bar)}
  [state]
  [:div {:id "net-bar"}])


;;;; ASSETS

(def no-value (constantly nil))


(defn asset-growth-chart [{:keys [transactions asset-values]}]
  (let [asset-data (fn [{:keys [transaction-type] :as t}]
                     (cond
                       (nil? transaction-type)
                       ((juxt date :value no-value no-value) t)
                       (not= :chart-helper transaction-type)
                       ((juxt date no-value :sum
                              (fn [{:keys [amount sum]}]
                                (str "Contrib Amount:" (s-number amount) " Total: " (s-number sum))))
                        t)
                       :else
                       ((juxt date no-value :sum
                              (fn [{:keys [amount sum]}]
                                (str "Contrib Amount:" (s-number amount) " Total: " (s-number (+ sum amount)))))
                        t)
                       ))]
    (draw-line-chart
     "asset-growth"
     (->> (concat transactions asset-values)
          (sort-by :timestamp)
          (map asset-data)
          (into [["date" "value" "total-contribution"
                  {:label nil :role "tooltip"}]])
          data-table)
     {:title "GROWTH"
      :height 400
      :interpolateNulls true})))


(rum/defc asset-growth < rum/static
  {:did-mount (wrap-args asset-growth-chart)}
  [asset]
  [:div {:id "asset-growth"}])


(rum/defc asset-history < rum/static
  [{:keys [transactions asset-values]}]
  [:table.table.is-narrow.is-fullwidth
   [:tbody
    (map-indexed
     (fn [i {:keys [transaction-type amount value timestamp]}]
        (if transaction-type
          [:tr {:key i}
           [:td (.toLocaleDateString (js/Date. timestamp))]
           [:td (name transaction-type)]
           [:td
            {:class (if (> amount 0) "has-text-primary" "has-text-danger")}
            (s-number amount)]]
          [:tr {:key i}
           [:td (.toLocaleDateString (js/Date. timestamp))]
           [:td "value adjust"]
           [:td (s-number value)]]))
     (->> transactions
          (filter #(not= :chart-helper (:transaction-type %)))
          (concat asset-values)
          (sort-by :timestamp)
          reverse))]])


(defmethod render-modal :asset [state]
  (let [asset (get-in state [:assets (:asset state)])
        {:keys [name total-contrib current-value asset-type]} asset]
    [:div.modal.is-active
     [:div.modal-background {:on-click hide-modal}]
     [:div.modal-content.box.
      [:h1.title.has-text-primary.has-text-centered name " (" asset-type ")"]
      (asset-growth asset)
      [:div.level
       [:div.level-item (info-box "Contrib" (s-number total-contrib))]
       [:div.level-item (info-box "Growth" (s-number (- current-value total-contrib)))]
       [:div.level-item (info-box "Value" (s-number current-value))]]
      (asset-history asset)]
     [:button.modal-close.is-large {:on-click hide-modal}]]))


(rum/defc assets < rum/static
  [{:keys [assets]}]
  (let [assets (filter (comp not :closed?) assets)
        closed-assets (filter :closed? assets)]
    [:div.columns.is-multiline.is-centered
     (map-indexed
      (fn [i {:keys [name asset-type current-value monthly-growth]}]
        [:div.column.is-2.has-text-centered.asset-button
         {:key i :on-click #(show-asset name)}
         [:p.heading.has-text-primary name]
         [:p.heading asset-type]
         [:p.heading (s-number current-value)]
         [:p.heading {:class (if (> monthly-growth 0) "has-text-primary" "has-text-danger")}
          (str (s-number monthly-growth) "%")]])
      (vals assets))]))

;;;; PAGES


(defmethod render-page :default [state]
  [:strong "No renderer for page " (str (:page state))])


(defmethod render-page :loading [state]
  (load-charts)
  [:div
   [:h1.title.has-text-primary.has-text-centered "Loading all the Things"]
   [:h2.subtitle.has-text-primary.has-text-centered "(╯°□°）╯︵ ┻━┻"]])


(defn col [size & children]
  [:div.column {:class (str "is-" size)}
   [:div.box.is-shadowless.has-text-grey-lighter
    children]])



(defmethod render-page :home [{:keys [current] :as state}]
  [:div
   (render-modal state)
   [:div.columns.is-multiline.is-centered
    (col 3 (info-box "WEALTH INDEX" (s-number (:wi current))))
    (col 3 (info-box "NET" (s-number (:net current))))
    (col 3 (info-box "MONTH TO DATE" (str (s-number (:monthly-growth current)) "%")))
    (col 3 (info-box "YEAR TO DATE" (str (s-number (:yearly-growth current)) "%")))
    (col 2 (wi-guage state))
    (col 6 (wi state))
    (col 4 (net state))
    (col 6 (salaries state))
    (col 12 (assets state))
    ]])


;;;;APP

(rum/defc app < rum/reactive [state]
  (let [current-state (rum/react state)]
    (render-page current-state)))


(rum/mount (app *state) (.getElementById js/document "app"))

;;;; DEV


(defn on-js-reload []
   (swap! *state update-in [:__figwheel_counter] inc)
)
