(ns bucks.core
  (:require [bucks.domain :as domain]
            [bucks.example :as example]
            [bucks.parse :as parse]
            [rum.core :as rum]
            [cljs-time.format :as time.format]
            [clojure.string :as string]
            [cljs-time.core :as time]
            [cljs-time.coerce :as time.coerce]))

(enable-console-print!)

;;;; STATE

(def initial-data (->> example/test-piped-csv
                       (parse/parse parse/pipe)
                       parse/as-domain-values
                       domain/all-your-bucks))

(defonce *state (atom {:page :loading
                       :delimiter parse/semicolon
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

(defn legend [position] {:textStyle {:color chart-base-color}
                         :position position})

(def h-axis {:gridlines {:color "none"}
             :textStyle {:color chart-base-color}})

(def v-axis {:gridlines {:color chart-base-color}
             :textStyle {:color chart-base-color}})

(defn draw-chart [Chart id data opt]
  (let [opt (merge
             {:animation {:startup true :duration 2000 :easing "out"}
              :colors ["#00d1b2" "#ff385f" "#ffde56" "#3372dd" "#EA7AF4" "#B43E8F" "#6200B3" "#3B0086"]
              :backgroundColor "none"
              :legend (legend "bottom")
              :titleTextStyle {:color chart-base-color}
              :hAxis h-axis
              :vAxis v-axis
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
              (merge
               {:pieHole 0.4
                :legend (legend "right")} opt)))


(defn draw-column-chart [id data opt]
  (draw-chart js/google.visualization.ColumnChart id data opt))


(defn draw-bar-chart [id data opt]
  (draw-chart js/google.visualization.BarChart id data opt))


(defn draw-combo-chart [id data opt]
  (draw-chart js/google.visualization.ComboChart id data opt))


(def alternate-chart-colors ["#ffde56" "#3372dd"])


;;;; COMPONENTS

(def number-formatter (js/Intl.NumberFormat.))


(defn format-num [v] (.format number-formatter v))


(defn format-% [v] (str (format-num v) "%"))


(defn color-num [n] (cond (= 0 n) "has-text-light"
                          (> n 0) "has-text-primary"
                          :else "has-text-danger"))


(defn note [& args]
  [:div.has-text-centered.has-text-grey
   [:small args]])


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
      {:title "GROWTH DISTRIBUTION"
       :colors alternate-chart-colors}))))

;;;; YEAR MODAL

(defn year-growth-chart [year start goals daily-values]
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
           growth (->> daily-values
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
        {:keys [daily-values goals start wi growth-months growth-year self-growth-percent
                transactions end transaction-growth-percent transaction-total salary] :as data}
        (get-in data [:years year])]
    (let [growth (- end start)]
      [:div
       [:h1.title.has-text-light.has-text-centered year]
       [:div.level
        (color-level-item "YTD" format-% growth-year)
        (color-level-item "WI" format-num wi)
        (color-level-item "SALARY" format-% salary)]
       (year-growth-chart year start goals daily-values)
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

(defn asset-chart [daily-values transactions]
  (chart
   "asset-chart"
   (fn [id]
     (let [transactions (domain/with-running-total transactions (-> daily-values last :cljs-date))]
       (draw-area-chart
        id
        (->> (map (juxt :date :value nothing) daily-values)
             (into (map (juxt :date nothing :total) transactions))
             (into [["date" "value" "transactions"]])
             data-table)
        {:title "GROWTH"})))))


(defn asset-group-item-pie [assets]
  (chart
   "asset-group-item-pie"
   (fn [id]
     (draw-pie-chart
      id
      (->> assets
           (sort-by :value)
           reverse
           (map (juxt :name :value))
           (into [["asset" "%"]])
           data-table)
      {:title "ASSET DISTRIBUTION"}))))


(defmethod render-modal :asset-group [{:keys [modal data]}]
  (let [asset-group (:data modal)
        {:keys [daily-values asset-type growth-month value assets self-growth-precentage
                growth-all-time growth-amount self-growth-amount growth-year
                transactions contribution-growth-amount]}
        (get-in data [:asset-groups asset-group])]
    [:div
     [:h1.title.has-text-light.has-text-centered asset-group]
     [:div.level
      (level-item "VALUE" format-num value)
      (level-item "CONTRIBUTIONS" format-num contribution-growth-amount)]
     [:div.level
      (color-level-item "ALL TIME" format-% self-growth-precentage)
      (color-level-item "YTD" format-% growth-year)
      (color-level-item "MTD" format-% growth-month)]
     (asset-chart daily-values transactions)
     (asset-group-item-pie assets)
     (growth-pie self-growth-amount contribution-growth-amount)]))


;;;; ASSETS MODAL

(defn asset-history [values transactions]
  (let [values (->> values
                    (map (fn [{:keys [date cljs-date value]}]
                           {:date date :cljs-date cljs-date :value value :note "value adjust" :class nil})))
        transactions (->> transactions
                          (map
                           (fn [{:keys [date cljs-date amount]}]
                             {:date date :cljs-date cljs-date :value amount
                              :note (if (pos? amount) "deposit" "withdrawal")
                              :class (color-num amount)})))
        coll (->> (into values transactions)
                  (sort-by :date)
                  reverse)]
    [:table.table.is-narrow.is-fullwidth
     [:tbody
      (map-indexed
       (fn [i {:keys [cljs-date value note class]}]
         [:tr {:key i :class class}
          [:td (time.format/unparse (time.format/formatter "yyyy-MM-dd") cljs-date)]
          [:td note]
          [:td (format-num value)]])
       coll)]]
    ))


(defmethod render-modal :asset [{:keys [modal data]}]
  (let [asset (:data modal)
        {:keys [daily-values asset-type include-in-net growth-month name value
                growth-all-time growth-amount closed? values self-growth-amount
                growth-year units transactions start-value contribution-growth-amount self-growth-precentage]}
        (get-in data [:assets asset])]
    [:div
     [:h1.title.has-text-light.has-text-centered
      asset (when (> units 0) (str " (" units ")"))]
     [:div.level
      (level-item "TYPE" identity asset-type)
      (level-item "VALUE" format-num value)
      (level-item "CONTRIBUTIONS" format-num contribution-growth-amount)]
     [:div.level
      (color-level-item "ALL TIME" format-% self-growth-precentage)
      (color-level-item "YTD" format-% growth-year)
      (color-level-item "MTD" format-% growth-month)]
     (asset-chart daily-values transactions)
     (growth-pie self-growth-amount contribution-growth-amount)
     (asset-history values transactions)]))

;;;; MAIN PAGE


(defn wealth-guage [{:keys [wi income-wi expense-wi]}]
  (chart
   "wi-guage"
   (fn [id]
     (draw-guages
      id
      (->> [["Level" "Value"]
            ["Salary WI" wi]
            (when income-wi ["Income WI" income-wi])
            (when expense-wi ["Expense WI" expense-wi])]
           (keep identity)
           data-table)
      {:height 200 :majorTicks (range 0 6 1) :max 5 :min 0
       :greenColor "#00d1b2" :greenFrom 3 :greenTo 5
       :yellowColor "#ffde56" :yellowFrom 1.5 :yellowTo 3
       :redColor "#ff385f" :redFrom 0 :redTo 1.5}))))


(defn color-wi-num [n] (cond (> 1.5 n) "has-text-danger"
                             (> 3 n) "has-text-warning"
                             :else "has-text-success"))


(defn wi-chart [daily-wi wi-goals]
  (chart
   "wi-chart"
   (fn [id]
     (let [goal-headings (->> wi-goals (map :name))
           headings (into ["month" "value"] goal-headings)
           get-values (->> (range (count wi-goals))
                           (map (constantly nothing))
                           (into [:date :wi])
                           (apply juxt)
                           )
           base-row (->> (+ 2 (count wi-goals))
                         range
                         (map nothing)
                         vec)
           goals
           (->> wi-goals
                (map-indexed
                 (fn [i {:keys [graph]}]
                   (let [[a b] graph
                         n (+ 2 i)]
                     [(assoc base-row
                             0 (:date a)
                             n (:wi a))
                      (assoc base-row
                             0 (:date b)
                             n (:wi b))])))
                (reduce into))
           growth (->> daily-wi
                       (map get-values))]
       (draw-area-chart
        id
        (->> growth
             (into goals)
             (into [headings])
             data-table)
        {:title "WEALTH INDEX"
         :explorer {:axis nil
                    :actions ["dragToZoom", "rightClickToReset"]
                    :keepInBounds true}
         :hAxis (assoc-in h-axis [:viewWindow :max] (js/Date.))})))))


(defn growth-chart [daily-wi]
  (chart
   "growth-chart"
   (fn [id]
     (draw-area-chart
      id
      (->> daily-wi
           (map (juxt :date :asset-value))
           (into [["month" "value"]])
           data-table)
      {:title "NET WORTH"}))))


(defn salaries-tooltip [{:keys [date value name]}]
  (str (format-num value) " " name " "  (.toLocaleDateString date "en-ZA")))


(defn salaries-chart [{:keys [income-expense salaries]}]
  (let [nothing (constantly nil)
        headers (if (empty? income-expense)
                  [["month" "salary" {:type "string" :role "tooltip"}]]
                  [["month" "salary" {:type "string" :role "tooltip"} "expense" "income" "savings"]])
        salaries (if (empty? income-expense)
                   (map (juxt :date :value :name) salaries)
                   (map (juxt :date :value :name nothing nothing nothing) salaries))
        income-expense (map (juxt :date nothing nothing :expense :income :savings) income-expense)]
    (chart
     "salaries-chart"
     (fn [id]
       (draw-area-chart
        id
        (->> (concat salaries [(-> salaries last (assoc 0 (js/Date.)))])
             (into income-expense)
             (into headers)
             data-table)
        {:title "MONEY"})))))


(defn four-percent-rule-chart [{:keys [four-percent-rule-over-time]}]
  (chart
   "four-percent-rule-chart"
   (fn [id]
     (draw-area-chart
      id
      (->> four-percent-rule-over-time
           (map (juxt :date :four-percent-rule-total))
           (into [["month" "value"]])
           data-table)
      {:title "4% RULE REQUIRED OVER TIME"}))))


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
   (->> asset-groups
        (sort-by first)
        (map-indexed
         (fn [i [name {:keys [growth-year growth-all-time value self-growth-precentage] :as d}]]
           [:div.column.is-2.has-text-centered.asset-button
            {:key i :on-click #(show-modal :asset-group name)}
            [:p.heading.has-text-light name]
            (when-not (= 0 value) [:p.heading (format-num value)])
            [:p.heading {:class (color-num growth-year)} (format-% growth-year) " ytd"]
            (when-not (= 0 value) [:p.heading {:class (color-num self-growth-precentage)} (format-% self-growth-precentage)])
            ])))])


(defn asset-group-pie [asset-groups]
  (chart
   "asset-distribution"
   (fn [id]
     (draw-pie-chart
      id
      (->> asset-groups
           vals
           (sort-by :value)
           reverse
           (map (juxt :asset-type :value))
           (into [["asset type" "%"]])
           data-table)
      {:title "ASSET DISTRIBUTION"}))))


(defn assets-per-person-pie [assets-per-person]
  (chart
   "assets-per-person"
   (fn [id]
     (draw-pie-chart
      id
      (->> assets-per-person
           (sort-by :value)
           reverse
           (map (juxt :owner :value))
           (into [["owner" "%"]])
           data-table)
      {:title "ASSETS PER PERSON"}))))


(defn asset-area-chart [type assets]
  (let [assets  (->> (vals assets)
                     (sort-by :value)
                     reverse)
        total   (count assets)
        base-row (->> (range (inc total)) (map (constantly nil)) vec)
        all-days (->> assets
                      (map :daily-values)
                      (reduce into)
                      (map :date)
                      set
                      sort)
        asset-mapping (->> assets
                           (map-indexed
                            (fn [i {:keys [daily-values]}]
                              [i
                               (->> daily-values
                                    (map
                                     (fn [{:keys [date value]}]
                                       [date value]))
                                    (into {}))]))
                           (into {}))
        rows (->> all-days
                    (map
                     (fn [d]
                       (->> assets
                            (map-indexed
                             (fn [i _]
                               (get-in asset-mapping [i d] 0)))
                            (#(conj % d))))))
        headers (->> assets
                     (map :asset-type)
                     (#(conj % "Date")))]
    (chart
     (str "asset-area-chart" type)
     (fn [id]
       (draw-area-chart
        id
        (->> rows
             (into [headers])
             data-table)
        {:title (str "ASSETS DISTRIBUTION (" type ")")
         :isStacked type
         :areaOpacity 0.9
         :curveType "function"})))))


(defn assets [assets]
  [:div.columns.is-multiline.is-centered
   (->> assets
        (sort-by :asset-type)
        (map-indexed
         (fn [i {:keys [name asset-type growth-year growth-all-time value self-growth-precentage] :as d}]
           [:div.column.is-2.has-text-centered.asset-button
            {:key i :on-click #(show-modal :asset name)}
            [:p.heading.has-text-light name]
            [:p.heading asset-type]
            (when-not (= 0 value) [:p.heading (format-num value)])
            (when-not (= 0 value) [:p.heading {:class (color-num growth-year)} (format-% growth-year) " ytd"])
            (when-not (= 0 value) [:p.heading {:class (color-num self-growth-precentage)} (format-% self-growth-precentage)])
            ])))])


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


(defn tfsa-lifetime [tfsa-tracking]
  (chart
   "tfsa-tracking"
   (fn [id]
     (let [headings ["person" "value"]
           limit    ["Limit" 500000]
           data (->> tfsa-tracking
                     vals
                     (map (juxt :owner :lifetime)))]
       (draw-bar-chart
        id
        (->> data
             (into [headings limit])
             data-table)
        {:title "TFSA LIFETIME LIMITS"})))))


(defn tfsa-yearly [tfsa-tracking]
  (chart
   "tfsa-yearly"
   (fn [id]
     (let [owners (->> tfsa-tracking
                       keys)
           headings (->> owners
                         (into ["Year" "Limit"]))
           years  (->> tfsa-tracking
                       vals
                       (map #(-> % :yearly keys))
                       (reduce into)
                       sort
                       set)
           yearly-data (->> years
                            (map
                             (fn [y]
                               (->> owners
                                    (map (fn [o]
                                           (get-in tfsa-tracking [o :yearly y])))
                                    (into [(js/Date. y 0 1) 33000])))))]
       (draw-combo-chart
        id
        (->> yearly-data
             (into [headings])
             data-table)
        {:title "TFSA YEARLY CONTRIBUTIONS"
         :seriesType "bars"
         :series {0 {:type "line"}}
         :hAxis {:gridlines {:color "none"}
                 :textStyle {:color chart-base-color}
                 :ticks (map #(js/Date. % 0 1) years)
                 :format "y"}
         :vAxis {:baseline 0
                 :gridlines {:color chart-base-color}
                 :textStyle {:color chart-base-color}}})))))


(defn four-percent-rule [data]
  [:div
   (info-box "CURRENT NET WORTH" (format-num (get-in data [:current-values :asset-value])))
   [:br]
   (info-box "4% RULE GOAL" (format-num (get-in data [:money-health :four-percent-rule-total])))
   [:br]
   (info-box "% of 4% REACHED" (format-% (get-in data [:money-health :percent-of-four-completed])))])


(defn time-till-independence [data]
  [:div
   (info-box "AVG MONTHLY EXPENSE " (format-num (get-in data [:money-health :avg-monthly-expense])))
   [:br]
   (info-box "AVG SAVING RATE " (format-% (get-in data [:money-health :avg-saving-rate] 0)))
   [:br]

   (info-box "YEARS TILL INDEPENDENCE"
             (str (format-num (get-in data [:money-health :years-till-financially-independent]))
                  " (age " (format-num (get-in data [:money-health :age-when-financially-independent])) ")"))
   [:br]
   (note "previous " domain/lookback-in-months " periods averaged")
   (note "assumes " domain/assumed-return-after-inflation "% return after inflation and "
         domain/assumed-safe-withdrawal-rate "% withdrawal rate")])


(defn savings-rate-chart [income-expense]
  (let [rows (->> income-expense
                  (map (juxt :date :recorded-saving-rate :expected-saving-rate )))
        headers ["Date" "Actual Recorded In Assets" "Expected Based On Expense"]]
    (chart
     (str "savings-rate-chart" type)
     (fn [id]
       (draw-area-chart
        id
        (->> rows
             (into [headers])
             data-table)
        {:title "SAVING RATE %"
         :isStacked type
         :areaOpacity 0
         })))))


(defn retire-years-fixing-saving-rate [data]
  [:table.table.is-narrow.is-fullwidth
   [:tbody
    [:tr [:td.has-text-centered "Saving Rate"] [:td.has-text-centered "Years Left"]]
    (map-indexed
     (fn [i {:keys [years saving-rate current]}]
       [:tr {:key i :class (cond current "has-text-info"
                                 (<= years 10) "has-text-primary"
                                 (<= years 20) "has-text-warning"
                                 (> years 20) "has-text-danger"
                                 :else "")}
        [:td.has-text-centered (format-% saving-rate)]
        [:td.has-text-centered years]])
     data)]])


(defn independence-years-tracking-chart [income-expense]
  (let [end-date #(time.coerce/to-date (time/plus (time.coerce/from-long (:timestamp %))
                                                  (time/years (:years-to-independence %))))
        first-entry (first income-expense)
        last-entry (last income-expense)
        rows (->> income-expense
                  (map (juxt :date :years-to-independence (constantly nil) (constantly nil)))
                  (into [[(:date first-entry) nil (:years-to-independence first-entry) nil]
                         [(end-date first-entry) nil 0 nil]
                         [(:date last-entry) nil nil (:years-to-independence last-entry)]
                         [(end-date last-entry) nil nil 0]]))
        headers ["Date" "Years" "Start Trajectory" "Actual Trajectory"]]
    [:div
     (chart
      (str "independence-years-tracking-chart" type)
      (fn [id]
        (draw-area-chart
         id
         (->> rows
              (into [headers])
              data-table)
         {:title "YEARS TO INDEPENDENCE"
          :isStacked type
          :areaOpacity 0
          })))
     (note "uses an avg window of " domain/lookback-in-months " periods around the entries to compensate for outliers")]))


(defn seperator
  ([] [:br])
  ([text]
   [:div.column.is-12
    [:div.level.is-marginless
     [:div.level-item
      [:p.heading.is-marginless text]]]]))


(defmethod render-page :main [{:keys [data modal]}]
  (let [{:keys [wi asset-value growth-month growth-year]} (:current-values data)]
    [:div.columns.is-multiline.is-centered
     (col 2 (info-box "WEALTH INDEX" (format-num wi) (color-wi-num wi)))
     (col 2 (info-box "NET" (format-num asset-value)))
     (col 2 (info-box "MONTH TO DATE" (format-% growth-month) (color-num growth-month)))
     (col 2 (info-box "YEAR TO DATE" (format-% growth-year) (color-num growth-year)))
     (col 2 (info-box "EM FUND MONTHS" (format-num (get-in data [:money-health :emergency-fund-ratio]))))
     (col 4 (wealth-guage (:current-values data)))
     (col 4 (wi-chart (:daily-wi data) (:wi-goals data)))
     (col 4 (growth-chart (:daily-wi data)))
     (seperator "Money Health")

     (col 6 (salaries-chart data))
     (col 6 (savings-rate-chart (:income-expense data)))

     (col 3 (time-till-independence data))
     (col 6 (independence-years-tracking-chart (:independence-years-tracking data)))
     (col 3 (retire-years-fixing-saving-rate (get-in data [:money-health :retire-years-fixing-saving-rate])))

     (col 3 (four-percent-rule data))
     (col 6 (four-percent-rule-chart (:money-health data)))

     (seperator "Assets")
     (col 4 (asset-group-pie (:asset-groups data)))
     (col 4 (assets-per-person-pie (:assets-per-person data)))
     (seperator)
     (col 6 (asset-area-chart "absolute" (:asset-groups data)))
     (col 6 (asset-area-chart "percent" (:asset-groups data)))
     (seperator "TAX FREE")
     (seperator
      (note "TFSA withdrawals are currently treated as transfers between tfsa accounts. Thus withdrawals that are not transfers will not reflect correctly."))
     (col 6 (tfsa-yearly (:tfsa-tracking data)))
     (col 6 (tfsa-lifetime (:tfsa-tracking data)))
     (seperator "Money Lifetime")
     [:div.column.is-12 (money-lifetimes (:money-lifetimes data))]
     (seperator "Years")
     (col 12 (years (:years data)))
     (seperator "Asset groups")
     (col 12 (asset-groups (:asset-groups data)))
     (seperator "Assets")
     (col 12 (assets (->> data :assets vals (filter (comp not :closed?)))))
     (seperator "Closed Assets")
     (col 12 (assets (->> data :assets vals (filter :closed?))))
     (seperator "drag graphs to zoom and double click to reset")
     ]))


;;;;FILE UPLOAD

(defn set-file-data [data] (swap! *state #(assoc-in % [:modal :data] data)))

(defn set-app-data [data]
  (swap! *state #(assoc % :data data)))

(defn set-delimiter [d] (swap! *state #(assoc % :delimiter d)))

(rum/defc rerender [] [:div])


(defmethod render-modal :upload [{:keys [modal delimiter]}]
  [:div.has-text-light
   [:h1.heading.has-text-centered "Choose Your File Delimiter"]
   [:div.buttons.is-centered
    (map-indexed
     (fn [i d]
       [:button.button
        {:key i
         :class (when (= delimiter d) "is-info")
         :on-click #(set-delimiter d)}
        d])
     parse/seperators)]
   [:form
    [:div.file.is-centered
     [:label.file-label
      [:input.file-input {:type "file" :name "storage"
                          :on-change (fn [e]
                                       (let [file (aget (.. e -target -files) 0)
                                             reader (js/FileReader.)]
                                         (set! (.-onload reader)
                                               #(set-file-data (.. % -target -result)))
                                         (.readAsText reader file)))}]
      [:span.file-cta
       [:span.file-icon
        [:i.fa.fa-upload]]
       [:span.file-label "Upload"]]]]]
   (when-let [content (:data modal)]
     (let [result
           (->> content
                (parse/parse delimiter))
           errors
           (->> result
                (filter (comp not :valid?))
                (map (juxt :i :error)))]
       (if-not (empty? errors)
         [:div.content
          [:hr]
          [:p.heading.has-text-centered.has-text-danger "Oh no you have the following errors"]
          [:ul
           (map-indexed
            (fn [i [row e]]
              [:li {:key i} "row: " row ": " e])
            errors)]]
         [:div.content
          [:hr]
          [:p.heading.has-text-centered.has-text-primary "Sweet your file has no errors"]
          [:div.buttons.is-centered
           [:button.button.is-primary
            {:on-click (fn [_]
                         (set-app-data (-> result
                                           parse/as-domain-values
                                           domain/all-your-bucks))
                         (hide-modal)
                         (page :loading)
                         (js/setTimeout #(page :main)))}
            "GO"]]])))])

;;;;APP

(defmethod render-modal :help [state]
  [:div.has-text-light
   [:p "To view your bucks, upload a csv file with your data. The file should have lines as described below.
        Each Item should be on a new line. Scroll down to see an example file.
        All data is local to your computer and is NOT uploaded anywhere."]
   [:br]
   [:ul {:style {:list-style "disc" :margin-left "20px"}}
    [:li "Your file can be delimited using either a pipe ( | ), comma ( , ) or semicolon ( ; )"]
    [:li.has-text-danger "Names cannot start with a number"]]
   [:hr]
   (map-indexed
    (fn [i [key values description]]
      [:div.has-text-light {:key i}
       [:strong.has-text-white (->> (string/split key "-")
                                    (map string/capitalize)
                                    (string/join " "))]
       [:p description]
       [:p.has-text-warning
        [:span.has-text-primary key] " | "
        (->> values
             (map name)
             (string/join " | "))]
       [:hr]
       ])
    domain/data-types-config)
   [:strong.has-text-white "Example"]
   [:br]
   [:pre
    [:code example/test-piped-csv]]])


(defn history
  ([version features breaking-changes] (history version nil features breaking-changes))
  ([version url features breaking-changes]
   (let [style {:list-style "disc" :margin-left "20px"}
         version-1 (if url version [version [:small " (current)"]])
         features (->> features
                       (map-indexed (fn [i f] [:li {:key i :style style} f]))
                       (into [[:li.has-text-info "Features"]]))
         breaking-changes (if (empty? breaking-changes)
                            []
                            (->> breaking-changes
                                 (map-indexed (fn [i f] [:li {:key (str "e" i)  :style style} f]))
                                 (into [[:li.has-text-danger "Breaking Changes"]])))]
     [:div
      [:h1.has-text-primary "V" version-1 (when url [:small [:a {:href url} " (view here)"]])]
      [:ul
       features
       breaking-changes]
      [:br]]
     )))


(defmethod render-modal :history [state]
  [:div.has-text-light
   (history
    "1.x"
    ["Todo Calculate RA Contributions"
     "Todo monthly transactions bar chart"
     "Todo Retirement goals chart"
     "Add actual independecy trend line"]
    [])
   (history
    "1.16"
    "/bucks-v1.16"
    ["Added Asset Type Distributions over time graphs"
     "Calculate Average Savings Rate"
     "Graph Expected and Actual Savings Rate over time"
     "Calculate Estimate Years To Retirement https://www.mrmoneymustache.com/2012/01/13/the-shockingly-simple-math-behind-early-retirement/"
     "Show Estimate Years To retirement for different savings rates"
     "Graph Years to Independence over time with goal line"
     "Improve Pie Chart Legend"
     "Improve Wealth Index Goal Chart"
     "Change Lookback window from 6 Months to 12 Months"]
    [])
   (history
    "1.7"
    "/bucks-v1.7"
    ["Track TFSA seperately from asset types. To view a TFSA asset in the TFSA section set 'is-tfsa' to 'y'"
     "Allow Multiple Salaries"
     "Added 'Actively Managed' asset type"
     "Allow 'comment' rows"]
    ["'open-asset' requires an 'is-tfsa' field to be added to track TFSA's seperately. Can be 'y' or 'n'. Add to the end of the row"
     "'salaries' requires an 'owner' field. This is the first field after 'salaries'"])
   (history
    "1.3"
    "/bucks-v1.3"
    ["Tax Free Savings Reporting"
     "Monthly Income and Expense Tracking. Use the 'income-expense' row."
     "Retirement Numbers based on the 4% rule. Requires monthly income and expense."
     "'Emergency Fund' asset type. Used to calculate 'MONTHS COVERED BY EMERGENCY FUND'"
     "Wealth Distribution by owne"]
    ["'open-asset' requires an 'owner' field to be added to track TFSA's"])
   (history
    "1"
    "/bucks-v1"
    ["Initial Release"]
    [])])


(rum/defc app < rum/reactive [state]
  (let [current-state (rum/react state)]
    [:div
     [:nav.navbar.is-black
      [:div.navbar-menu
       [:div.navbar-end
        [:a.navbar-item {:on-click #(show-modal :help nil)}
         [:span.icon [:i.fa.fa-question-circle]]]
        [:a.navbar-item {:on-click #(show-modal :upload nil)}
         [:span.icon [:i.fa.fa-upload]]]
        [:a.navbar-item {:on-click #(show-modal :history nil)}
         [:span.icon [:i.fa.fa-history]]]]]]
     (when-not (= :hidden (get-in current-state [:modal :key]))
       [:div.modal.is-active
        [:div.modal-background {:on-click hide-modal}]
        [:div.modal-content
         [:div.box
          (render-modal current-state)]]
        [:button.modal-close.is-large {:on-click hide-modal}]])
     [:div.section
      (render-page current-state)]]))


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

(. js/document (getElementById ""))
