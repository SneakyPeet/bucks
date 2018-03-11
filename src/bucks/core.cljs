(ns bucks.core
  (:require [bucks.domain :as domain]
            [bucks.example :as example]
            [bucks.parse :as parse]
            [rum.core :as rum]
            [cljs-time.format :as time.format]
            [clojure.string :as string]))

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

(defn draw-chart [Chart id data opt]
  (let [opt (merge
             {:animation {:startup true :duration 2000 :easing "out"}
              :colors ["#00d1b2" "#ff385f" "#ffde56" "#3372dd" "#EA7AF4" "#B43E8F" "#6200B3" "#3B0086"]
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
                    :keepInBounds true}})))))



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


(defn seperator [text]
  [:div.level.is-marginless
   [:div.level-item
    [:p.heading.is-marginless text]]])


(defmethod render-page :main [{:keys [data modal]}]
  (let [{:keys [wi asset-value growth-month growth-year]} (:current-values data)]
    [:div.columns.is-multiline.is-centered
     (col 3 (info-box "WEALTH INDEX" (format-num wi) (color-wi-num wi)))
     (col 3 (info-box "NET" (format-num asset-value)))
     (col 3 (info-box "MONTH TO DATE" (format-% growth-month) (color-num growth-month)))
     (col 3 (info-box "YEAR TO DATE" (format-% growth-year) (color-num growth-year)))
     (col 2 (wealth-guage wi))
     (col 7 (wi-chart (:daily-wi data) (:wi-goals data)))
     (col 3 (asset-group-pie (:asset-groups data)))
     (col 6 (growth-chart (:daily-wi data)))
     (col 6 (salaries-chart (:salaries data)))
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
       (prn (->> result
                 (filter (comp not :valid?))))
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
   [:p "To view your bucks upload a csv file with your data. The file can have lines as described below.
        Each Item should be on a new line. Scroll down to see an example file.
        All data is local to your computer and is NOT uploaded anywhere."]
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


(rum/defc app < rum/reactive [state]
  (let [current-state (rum/react state)]
    [:div
     [:nav.navbar.is-black
      [:div.navbar-menu
       [:div.navbar-end
        [:a.navbar-item {:on-click #(show-modal :help nil)}
         [:span.icon [:i.fa.fa-question-circle]]]
        [:a.navbar-item {:on-click #(show-modal :upload nil)}
         [:span.icon [:i.fa.fa-upload]]]]]]
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
