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


(defn show-year [y]
  (swap! *state #(assoc %
                        :modal :yearly
                        :year y)))


(def number-formatter (js/Intl.NumberFormat.))


(defn s-number [v] (.format number-formatter v))


(defn s-percent [v] (str (s-number v) "%"))


(defn date [m] (js/Date. (:timestamp m)))


(defn pos-neg-class [v]
  (if (neg? v) "has-text-danger" "has-text-primary"))


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
    (draw-area-chart
     "wi-chart"
     (->> wi
          (sort-by :timestamp)
          (map (fn [{:keys [timestamp goals]}]
                 (conj goals (js/Date. timestamp))))
          (into [headings])
          data-table)
     {:title  "WEALTH INDEX"
      :height 250
      :series {1 {:areaOpacity 0}}})))


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

(defn net-chart [{:keys [wi]}]
  (draw-area-chart
   "net-chart"
   (->> wi
        (map (juxt date :net))
        (into [["month" "value"]])
        data-table)
   {:title "GROWTH"}))

(rum/defc net < rum/static
  {:did-mount (wrap-args net-chart)}
  [state]
  [:div {:id "net-chart"}])


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
    (draw-area-chart
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
        {:keys [name total-contrib current-value asset-type
                monthly-growth yearly-growth overall-growth]} asset]
    [:div.modal.is-active
     [:div.modal-background {:on-click hide-modal}]
     [:div.modal-content.box
      [:h1.title.has-text-light.has-text-centered name " (" asset-type ")"]
      (asset-growth asset)
      [:div.level
       [:div.level-item (info-box "Contrib" (s-number total-contrib))]
       [:div.level-item (info-box "Growth" (s-number (- current-value total-contrib)))]
       [:div.level-item (info-box "Value" (s-number current-value))]]
      [:div.level
       [:div.level-item (info-box "Month To Date" (s-percent monthly-growth))]
       [:div.level-item (info-box "Year To Date" (s-percent yearly-growth))]
       [:div.level-item (info-box "All Time" (s-percent overall-growth))]]

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


(defn asset-distribution-pie [{:keys [assets]}]
  (draw-pie-chart
   "asset-dist"
   (->> assets
        vals
        (group-by :asset-type)
        (map
         (fn [[k v]]
           [k (->> v (map :current-value) (reduce + 0))]))
        (into [["asset" "value"]])
        data-table)
   {:title "ASSET DISTRIBUTION"}))


(rum/defc asset-distribution < rum/static
  {:did-mount (wrap-args asset-distribution-pie)}
  [state]
  [:div {:id "asset-dist"}])


;;;; GOALS
(rum/defc goal-table [monthly-values goal-start goals]
  [:table.table.is-narrow.is-fullwidth
   [:thead
    [:tr [:th.has-text-centered {:col-span (+ 2 (count goals))} "Monthly Targets"]]
    [:tr
     [:th "Month"] [:th "Actual"]
     (map-indexed
      (fn [i {:keys [name percentage]}]
        [:th {:key i} name " (" (s-percent percentage) ")"])
      goals)
     ]]
   [:tbody
    (map-indexed
     (fn [i {:keys [net timestamp]}]
       (let [prev-net (if (= 0 i)
                        goal-start
                        (:net (nth monthly-values (- i 1))))
             diff      (- net prev-net)]
         [:tr {:key i}
          [:td (+ 1 (.getMonth (js/Date. timestamp)))]
          [:td diff]
          (map-indexed
           (fn [j {:keys [per-month]}]
             (let [growth (+ (- per-month) diff)]
               [:td.has-text-center
                {:key (str "f" j)
                 :class (if (neg? growth) "has-text-danger" "has-text-primary")}
                (s-number growth)]))
           goals)]))
     monthly-values)]])


(defn goal-chart [{:keys [year goals goal-start monthly-values]}]
  (draw-area-chart
   "goal-chart"
   (->> (conj monthly-values {:net goal-start :timestamp (.getTime (js/Date. year 0 1))})
        (map
         (fn [{:keys [timestamp net]}]
           (into
            [(js/Date. timestamp) (- net goal-start)]
            (map (constantly nil) goals))))
        (into
         (map-indexed
          (fn [i _]
            (concat
             [(js/Date. year 0 1) nil]
             (map (constantly nil) (range i))
             [0]
             (map (constantly nil) (range (- (count goals) (+ 1 i))))))
          goals))
        (into
         (map-indexed
          (fn [i {:keys [end]}]
            (concat
             [(js/Date. year 11 31) nil]
             (map (constantly nil) (range i))
             [(- end goal-start)]
             (map (constantly nil) (range (- (count goals) (+ 1 i))))))
          goals))
        (into [(concat ["Date" "Value"] (map :name goals))])
        data-table)
   {:title "Goals"
    :interpolateNulls true
    :series (->> goals
                 (map-indexed
                  (fn [i _]
                    [(inc i) {:areaOpacity 0}]))
                 (into {}))}))


(rum/defc goal-charts < rum/static
  {:did-mount (wrap-args goal-chart)}
  [yearly]
  [:div {:id "goal-chart"}])


(defn goal-transactions-chart [{:keys [monthly-values goal-start]}]
  (draw-column-chart
   "goal-transactions"
   (->> monthly-values
        (map-indexed
         (fn [i {:keys [timestamp net transaction-amount]}]
           (let [prev-net (if (= 0 i) goal-start (:net (nth monthly-values (dec i))))
                 growth (- net prev-net)
                 self-growth (- growth transaction-amount)]
             (if (= transaction-amount (- self-growth))
               [(js/Date. timestamp) 0 0]
               [(js/Date. timestamp) transaction-amount self-growth]))))
        (into [["date" "transactions" "growth"]])
        data-table)
   {:title "Contributions"
    :isStacked true
    :colors ["#ffde56" "#3372dd"]
    :trendlines {0 {:color "#00ffd4"}}}))


(rum/defc goal-transactions < rum/static
  {:did-mount (wrap-args goal-transactions-chart)}
  [yearly]
  [:div {:id "goal-transactions"}])

(defn goal-expectations [{:keys [goals net]}]
  [:div.level
   [:div.level-item
    [:div.has-text-centered
     [:p.heading "Actual"]
     [:p.title.has-text-light (s-number net)]]]
   (map-indexed
    (fn [i {:keys [name growth]}]
      [:div.level-item {:key i}
       [:div.has-text-centered
        [:p.heading name]
        [:p.title {:class (if (>= net growth) "has-text-primary" "has-text-danger")}
         (s-number growth)]]])
    goals)])


(defn goal-overview [{:keys [growth wi-growth salary-growth]}]
  [:div.level
   [:div.level-item
    [:div.has-text-centered
     [:p.heading "YTD"]
     [:p.title
      {:class (pos-neg-class growth)}
      (s-percent growth)]]]
   [:div.level-item
    [:div.has-text-centered
     [:p.heading "WI"]
     [:p.title
      {:class (pos-neg-class wi-growth)}
      (s-number wi-growth)]]]
   [:div.level-item
    [:div.has-text-centered
     [:p.heading "Salary"]
     [:p.title
      {:class (pos-neg-class salary-growth)}
      (s-percent salary-growth)]]]])


(defmethod render-modal :yearly [state]
  (let [{:keys [year goals growth monthly-values goal-start] :as yearly}
        (get-in state [:yearly-goals (:year state)])]
    [:div.modal.is-active
     [:div.modal-background {:on-click hide-modal}]
     [:div.modal-content.box
      [:h1.title.has-text-light.has-text-centered year]
      (goal-overview yearly)
      (goal-charts yearly)
      (goal-expectations yearly)
      (goal-transactions yearly)
      (goal-table monthly-values goal-start goals)
     [:button.modal-close.is-large {:on-click hide-modal}]]]))


(rum/defc goals < rum/static
  [{:keys [yearly-goals]}]
  [:div.columns.is-multiline.is-centered
   (map-indexed
    (fn [i year]
      [:div.column.is-1.has-text-centered.asset-button
       {:key i :on-click #(show-year year)}
       [:p.title.is-5 year]])
    (keys yearly-goals))])

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
    (col 3 (asset-distribution state))
    (col 7 (net state))
    (col 6 (wi state))
    (col 6 (salaries state))
    (col 12 (goals state))
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
