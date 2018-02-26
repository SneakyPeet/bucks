(ns bucks.core
  (:require [bucks.domain :as domain]
            [bucks.example :as example]
            [bucks.parse :as parse]
            [rum.core :as rum]
            [cljs-time.format :as time.format]))

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


(def alternate-chart-colors ["#ffde56" "#3372dd"])


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


(defn level-item [heading format value & [class key]]
  [:div.level-item (if key {:key key} {})
   [:div.has-text-centered
    [:p.heading heading]
    [:p.title
     {:class (or class "has-text-light")}
     (format value)]]])


(defn color-level-item [heading format value]
  [:div.level-item
   [:div.has-text-centered
    [:p.heading heading]
    [:p.title
     {:class (color-num value)}
     (format value)]]])


(def nothing (constantly nil))


(defn growth-pie [self-growth-precent transaction-growth-percent]
  (chart
   "growth-pie"
   (fn [id]
     (draw-pie-chart
      id
      (data-table
       [["type" "%"]
        ["growth by contributions" (max 0 transaction-growth-percent)]
        ["growth by interest" (max 0 self-growth-precent)]])
      {:colors alternate-chart-colors}))))

;;;; YEAR MODAL

(defn year-growth-chart [year start goals growth-months]
  (chart
   "year-growth-chart"
   (fn [id]
     (let [goal-headings (->> goals (map :name))
           headings (into ["month" "value"] goal-headings)
           get-values (->> (range (count goals))
                           (map (constantly nothing))
                           (into [:date :value])
                           (apply juxt)
                           )
           start-date (js/Date. year 0 1)
           end-date (js/Date. year 11 31)
           base-goal-row (->> (+ 2 (count goals))
                              range
                              (map nothing)
                              vec)
           goal-start-row (assoc base-goal-row 0 start-date)
           goal-end-row (assoc base-goal-row 0 end-date)
           growth-start-row (assoc goal-start-row 1 start)
           goals
           (->> goals
                (map-indexed
                 (fn [i {:keys [start end]}]
                   (let [n (+ 2 i)]
                     [(assoc goal-start-row n start)
                      (assoc goal-end-row n end)])))
                (reduce into))
           growth (->> growth-months
                       (map get-values)
                       (#(conj % growth-start-row)))]
       (draw-area-chart
        id
        (->> growth
             (into goals)
             (into [headings])
             data-table)
        {:title "GOALS"})))))


(defn year-transactions-chart [growth-months]
  (chart
   "year-transaction-chart"
   (fn [id]
     (draw-column-chart
      id
      (->> growth-months
           (map (juxt :date :transacted-amount :self-growth-amount))
           (into [["month" "transactions" "interest"]])
           data-table)
      {:title "CONTRIBUTIONS"
       :isStacked true
       :colors alternate-chart-colors
       :trendlines {0 {:color (first alternate-chart-colors)}
                    1 {:color (second alternate-chart-colors)}}
       :vAxis {:baselineColor "red"}}))))


(defn year-monthly-targets [goals growth-months]
  (when-not (empty? goals)
    [:div
     [:table.table.is-narrow.is-fullwidth
      [:thead
       [:tr [:th.has-text-centered {:col-span (+ 2 (count goals))} "Monthly Targets"]]
       [:tr
        [:th "Month"] [:th "Growth"]
        (map-indexed
         (fn [i {:keys [name]}]
           [:th {:key i} name])
         goals)]]
      [:tbody
       (map-indexed
        (fn [i {:keys [cljs-date growth-amount]}]
          [:tr {:key i}
           [:td (time.format/unparse (time.format/formatter "MMM") cljs-date)]
           [:td (format-num growth-amount)]
           (map-indexed
            (fn [i {:keys [expected-monthly]}]
              (let [diff (js/Math.floor (- growth-amount expected-monthly))]
                [:td {:key i :class (color-num diff)} diff]))
            goals)])
        growth-months)]]
     [:small.has-text-grey
      "* goal values indicate the difference between required monthly and actual monthly growth"]]))


(defmethod render-modal :year [{:keys [modal data]}]
  (let [year (:data modal)
        {:keys [monthly-values goals start wi growth-months growth-year self-growth-percent
                transactions end transaction-growth-percent transaction-total salary] :as data}
        (get-in data [:years year])]
    (let [growth-months (map domain/end-of-month growth-months)
          growth (- end start)]
      [:div
       [:h1.title.has-text-light.has-text-centered year]
       [:div.level
        (color-level-item "YTD" format-% growth-year)
        (color-level-item "WI" format-num wi)
        (color-level-item "SALARY" format-% salary)]
       (year-growth-chart year start goals growth-months)
       [:div.level.space
        (level-item "ACTUAL" format-num growth)
        (map-indexed
         (fn [i {:keys [name start end]}]
           (let [goal-growth (- end start)]
             (level-item (str "Required for " name) format-num goal-growth (color-num (- growth goal-growth)) i)))
         goals)]
       (year-transactions-chart growth-months)
       (growth-pie self-growth-percent transaction-growth-percent)
       (year-monthly-targets goals growth-months)])))


;;;; ASSET-GROUP MODAL

(defn asset-chart [monthly-values transactions]
  (chart
   "asset-chart"
   (fn [id]
     (let [monthly-values (->> monthly-values
                               (map domain/end-of-month))
           transactions (domain/with-running-total transactions (-> monthly-values last :cljs-date))]
       (draw-area-chart
        id
        (->> (map (juxt :date :value nothing) monthly-values)
             (into (map (juxt :date nothing :total) transactions))
             (into [["date" "value" "transactions"]])
             data-table)
        {:title "GROWTH"})))))


(defmethod render-modal :asset-group [{:keys [modal data]}]
  (let [asset-group (:data modal)
        {:keys [monthly-values asset-type growth-month value
                growth-all-time growth-amount self-growth-amount growth-year
                transactions contribution-growth-amount]}
        (get-in data [:asset-groups asset-group])]
    [:div
     [:h1.title.has-text-light.has-text-centered asset-group]
     [:div.level
      (level-item "VALUE" format-num value)
      (color-level-item "ALL TIME" format-% growth-all-time)
      (color-level-item "YTD" format-% growth-year)
      (color-level-item "MTD" format-% growth-month)]
     (asset-chart monthly-values transactions)
     [:small.has-text-grey
      "* Values are calculated monthly and might result in lag between the transaction and value"]
     (growth-pie self-growth-amount contribution-growth-amount)]))


;;;; MAIN PAGE


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
    (fn [i [year {:keys [growth-year] :as d}]]
      [:div.column.is-1.has-text-centered.asset-button
       {:key i :on-click #(show-modal :year year)}
       [:p.title.is-5.has-text-light year]
       [:p.subtitle.is-6 {:class (color-num growth-year)} (format-% growth-year)]])
    years)])


(defn asset-groups [asset-groups]
  [:div.columns.is-multiline.is-centered
   (map-indexed
    (fn [i [name {:keys [growth-month] :as d}]]
      [:div.column.is-2.has-text-centered.asset-button
       {:key i :on-click #(show-modal :asset-group name)}
       [:p.heading.has-text-light name]
       [:p.heading {:class (color-num growth-month)} (format-% growth-month)]
       ])
    asset-groups)])


(defn assets [assets]
  [:div.columns.is-multiline.is-centered
   (map-indexed
    (fn [i {:keys [name asset-type growth-month value] :as d}]
      [:div.column.is-2.has-text-centered.asset-button
       {:key i :on-click #(show-modal :asset-group name)}
       [:p.heading.has-text-light name]
       [:p.heading asset-type]
       (when-not (= 0 value) [:p.heading (format-num value)])
       (when-not (= 0 value) [:p.heading {:class (color-num growth-month)} (format-% growth-month)])
       ])
    assets)])


(defn money-lifetimes [lifetimes]
  [:div.columns.is-multiline.is-centered
   (map-indexed
    (fn [i {:keys [years months percent-of-salary inflation asset-growth] :as d}]
      (col 2
           [:div.has-text-centered
            (if (>= years 50)
              [:p.title.is-5.has-text-light.is-marginless "> 50 years"]
              [:p.title.is-5.is-marginless {:style {:color "grey"}}
               [:span.has-text-light years] "y " [:span.has-text-light months] "m"])
            [:p.heading.is-marginless percent-of-salary "% salary"]
            [:p.heading.is-marginless asset-growth "% growth"]
            [:p.heading.is-marginless inflation "% inflation"]]
       ))
    lifetimes)])


(defmethod render-page :main [{:keys [data modal]}]
  (let [{:keys [wi asset-value growth-month growth-year]} (:current-values data)]
    [:div.columns.is-multiline.is-centered
     (col 3 (info-box "WEALTH INDEX" (format-num wi) (color-wi-num wi)))
     (col 3 (info-box "NET" (format-num asset-value)))
     (col 3 (info-box "MONTH TO DATE" (format-% growth-month) (color-num growth-month)))
     (col 3 (info-box "YEAR TO DATE" (format-% growth-year) (color-num growth-year)))
     [:div.column.is-2.has-text-centered (wealth-guage wi)]
     ;;todo asset groups
     (col 7 (growth-chart (:monthly-wi data)))
     ;;wi
     (col 7 (salaries-chart (:salaries data)))
     [:div.column.is-12 (money-lifetimes (:money-lifetimes data))]
     (col 12 (years (:years data)))
     (col 12 (asset-groups (:asset-groups data)))
     (col 12 (assets (->> data :assets vals (filter (comp not :closed?)))))
     (col 12 (assets (->> data :assets vals (filter :closed?))))
     ]))


;;;;APP

(rum/defc app < rum/reactive [state]
  (let [current-state (rum/react state)]
    [:div
     (when-not (= :hidden (get-in current-state [:modal :key]))
       [:div.modal.is-active
        [:div.modal-background {:on-click hide-modal}]
        [:div.modal-content
         [:div.box
          (render-modal current-state)]]
        [:button.modal-close.is-large {:on-click hide-modal}]])
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
