(ns bucks.domain
  (:require [cljs.spec.alpha :as s]
            [cljs-time.core :as time]
            [cljs-time.coerce :as time.coerce]
            [clojure.string :as string]))


;;;; DEFAULTS

(def asset-types #{"TFSA" "RA" "Crypto" "Savings" "Shares" "CFD" "ETF" "UnitTrust" "Other" "Emergency Fund" "Actively Managed" "Cash"})
(def yes "y")
(def no "n")


;;;; PREDICATES

(defn not-empty-string? [s] (and (string? s)
                                 (not-empty s)))


(defn year? [n] (boolean (and (number? n) (>= n 1900) (> 3000 n))))


(defn month? [n] (boolean (and (number? n) (>= n 1) (> 13 n))))


(defn day? [n] (boolean (and (number? n) (>= n 1) (> 32 n))))


(defn percentage? [n] (boolean (and (number? n) (>= n 0) (>= 100 n))))


;;;; SPECS

(s/def :d/not-empty-string not-empty-string?)

(s/def :d/name :d/not-empty-string)

(s/def :d/year year?)

(s/def :d/month month?)

(s/def :d/day day?)

(s/def :d/amount number?)

(s/def :d/value number?)

(s/def :d/age pos?)

(s/def :d/percentage percentage?)

(s/def :d/units number?)

(s/def :d/wealth-index pos?)

(s/def :d/asset-type #(contains? asset-types %))

(s/def :d/include-in-net #(contains? #{yes no} %))

(s/def :d/inflation (comp not neg?))

(s/def :d/percent-of-salary pos?)

(s/def :d/asset-growth number?)

(s/def :d/owner string?)

(s/def :d/income pos?)

(s/def :d/expense pos?)

(s/def :d/is-tfsa #(contains? #{yes no} %))


;;;; DATA TYPES

(def data-types-config
  [["date-of-birth"
    [:d/year :d/month :d/day]
    "Your date of birth. Used in the wealth index calculations."]
   ["salary"
    [:d/owner :d/name :d/year :d/month :d/day :d/value]
    "A Salary Change. Name is the Name Of Employer. Value is your Monthly Salary Before Tax."]
   ["income-expense"
    [:d/year :d/month :d/income :d/expense]
    "Spend less than you earn. Save the Difference. Income is the money you have left after tax (Basically the money you budget). Expense is the money you spend. This will be used to calculate retirement things so keep that in mind when capturing expense."]
   ["open-asset"
    [:d/name :d/year :d/month :d/day :d/asset-type :d/value :d/units :d/include-in-net :d/owner :d/is-tfsa]
    (str "A new Asset. Name is the name of the asset. Asset-type should be one of the following " (string/join ", " asset-types)
         ". Requires opening value and units (leave as 0 if not relevant). Include in Net indicates if an asset should be included in the wealth index and total asset value calculations (can be y or n). Owner is the person the asset belongs to. Use 'is-tfsa' to see an asset in the tax free saving section (can be y or n)")]
   ["close-asset"
    [:d/name :d/year :d/month :d/day :d/value]
    "Close an existing asset. Name is the name of the asset. Value is the value of the asset before it closed"]
   ["transaction"
    [:d/name :d/year :d/month :d/day :d/amount :d/value :d/units]
    "An Asset Deposit or Withdrawal. Name is the Asset Name. Amount is the Amount of the deposit or Withdrawal. Value is the value of the asset After the transaction was made. Units are the amount of units transacted if applicable (like x bitcoin)"]
   ["value"
    [:d/name :d/year :d/month :d/day :d/value]
    "The Value of an asset at a certain point in time. Used for tracking asset growth ideally on a monthly or weekly basis. Name is the Asset Name. Value is the value of the asset at that point in time."]
   ["wi-goal"
    [:d/wealth-index :d/age]
    "A goal wealth index to reach at an age"]
   ["year-goal"
    [:d/year :d/percentage]
    "A growth percentage goal for a given year"]
   ["money-lifetime"
    [:d/inflation :d/percent-of-salary :d/asset-growth]
    "Calculates the lifetime of your money given annual inflation and asset growth compounded monthly with monthly withdrawals of a percentage of your salary."]
   ["comment"
    []
    "A row used for any kind of comment"]])


(def data-types (->> data-types-config
                     (map (juxt first second))
                     (into {})))


;;;; QUERY HELPERS

(defn type-of? [data-type m]
  (= data-type (:data-type m)))


(defn type-of-f? [data-type] (partial type-of? data-type))


(defn types-of-f? [& types]
  (fn [m] (->> types
               (map #(type-of? % m))
               (filter true?)
               not-empty)))


(defn timestamped [{:keys [year month day] :as m}]
  (let [date (js/Date. year (dec month) day)] ;;js months start at 0
    (assoc m
           :date date
           :cljs-date (time/date-time year month day)
           :timestamp (.getTime date))))


(defn cljs-timestamped [{:keys [cljs-date] :as m}]
  (assoc m
         :date (time.coerce/to-date cljs-date)
         :timestamp (time.coerce/to-long cljs-date)))


(defn end-of-month [{:keys [cljs-date] :as m}]
  (-> m
      (update :cljs-date time/last-day-of-the-month)
      cljs-timestamped))

(defn end-of-day []
  (-> (time/now)
      time/at-midnight
      (time/plus (time/days 1))))

(defn wrap-age [{:keys [timestamp] :as birthday}]
  (assoc birthday
         :age (time/in-years (time/interval
                              (time.coerce/from-long timestamp)
                              (end-of-day)))))

(defn daily-values
  "Calculates daily values from first item to now.
  Key is the map value containing the values"
  [k coll]
  (let [sorted-coll (sort-by :timestamp coll)
        now         (end-of-day)
        start       (first sorted-coll)]
    (if (empty? sorted-coll)
      []
      (loop [day          (:cljs-date start)
             value          (get start k)
             remaining-coll (rest sorted-coll)
             result         []]
        (let [future? (time/after? day now)
              next (first remaining-coll)
              new? (and (not (empty? remaining-coll))
                        (time/after? day (:cljs-date next)))
              next-day (time/plus day (time/days 1))
              next-value (if new? (get next k) value)
              next-result (conj result {:cljs-date day :value next-value})
              next-changes (if new? (drop 1 remaining-coll) remaining-coll)]
          (if-not future?
            (recur next-day next-value next-changes next-result)
            (map cljs-timestamped result)))))))


(defn wi [value salary age] (/ (/ value (* 12 salary)) (/ age 10)))


(defn growth-percentage
  ([] 0)
  ([x] 0)
  ([start end]
  (if (and (number? start) (number? end) (not= 0 end) (not= 0 start))
    (* 100 (- (/ end start) 1))
    0)))


(defn growth-all-time [values]
  (->> ((juxt first last) values)
       (map :value)
       (apply growth-percentage)))


(defn growth-year [values]
  (let [year (-> (end-of-day)
                 time/year
                 (time/date-time)
                 (time/minus (time/millis 1)))]
    (->> values
         (filter #(time/after? (:cljs-date %) year))
         growth-all-time)))


(defn growth-month [values]
  (->> values
       (take-last 2)
       growth-all-time))


(defn growth-amount [month-values]
  (- (:value (last month-values))
     (:value (first month-values))))


(defn contribution-amount [transactions]
  (->> transactions
       (map :amount)
       (reduce + 0)))


(defn with-year [m] (assoc m :year (time/year (:cljs-date m))))


(defn group-by-year [coll]
  (->> coll
       (map with-year)
       (group-by :year)))


(defn with-month [m] (assoc m :month (time/month (:cljs-date m))))


(defn group-by-month [coll]
  (->> coll
       (map with-month)
       (group-by :month)))


(defn with-running-total [transactions cljs-date]
  (loop [transactions transactions
         total 0
         result [(-> transactions first
                     (assoc :total 0)
                     (update :cljs-date #(time/minus % (time/minutes 1))))]]
    (if (empty? transactions)
      (map cljs-timestamped result)
      (let [[a b] transactions
            total (or (+ total (:amount a)) 0)
            base (assoc a :total total)
            transaction-group
            (if (nil? b)
              [base (assoc base :cljs-date cljs-date)]
              [base (assoc base :cljs-date (-> b :cljs-date (time/minus (time/minutes 1))))])]
        (recur
         (rest transactions)
         total
         (into result transaction-group)
         )))))


(defn unitize [starting-units daily-values transactions]
  (let [as-date (fn [{:keys [cljs-date]}] (time/at-midnight cljs-date))
        as-date-timestamp (comp time.coerce/to-long as-date)
        transaction-lookup (->> transactions
                                (filter (fn [{:keys [data-type]}]
                                          (or (nil? data-type) (= :transaction data-type))))
                                (group-by as-date-timestamp)
                                (map (fn [[t v]]
                                       (let [amount (->> v (map :amount) (reduce +))]
                                         [t {:amount amount
                                             :units (->> v (map :units) (reduce +))
                                           ;;;this is a best effort assuming there will generally be only 1 transaction per day
                                             :value-before (- (->> v first :value) amount)}])))
                                (into {}))
        starting-units (if (> starting-units 0) starting-units 1000)
        starting-unit-price (/ (:value (first daily-values)) starting-units)
        unitized-values
        (loop [result []
               values daily-values
               units starting-units
               unit-price starting-unit-price]

          (if (empty? values)
            (sort-by :timestamp result)
            (let [{:keys [value] :as current} (first values)
                  transaction (get transaction-lookup (as-date-timestamp current))
                  added-amount (get transaction :amount 0)
                  value (get transaction :value-before value)
                  current-unit-price (if (= 0 units) 0 (/ value units))
                  units-to-add
                  (if transaction
                    #_(/ (:amount transaction) current-unit-price)
                    (if (= (:units transaction) 0)
                      (/ (:amount transaction) current-unit-price)
                      (:units transaction))
                    0)
                  current-units (+ units units-to-add)
                  entry (assoc current
                               :units current-units
                               :unit-price (if (= current-units units) current-unit-price unit-price))]
              (recur
               (conj result entry)
               (rest values)
               current-units
               current-unit-price))))

        unit-price-lookup (->> unitized-values
                               (map (juxt (comp time.coerce/to-long as-date) identity))
                               (into {}))
        today (last unitized-values)
        todays-unit-price (:unit-price today)
        todays-date (as-date today)
        month (time/first-day-of-the-month todays-date)
        growth-since (fn [start-date end-date]
                       (let [start (get unit-price-lookup (time.coerce/to-long start-date))
                             end (get unit-price-lookup (time.coerce/to-long end-date))]
                         (when (and start end)
                           {:performance (growth-percentage (:unit-price start) (:unit-price end))
                            :overall (growth-percentage (:value start) (:value end))})))
        growth-years-rolling (fn [y] ;https://www.investopedia.com/terms/a/annualized-total-return.asp
                               (when (growth-since (time/minus todays-date (time/years y)) todays-date)
                                 (loop [x y
                                        end-date todays-date
                                        result []
                                        ]
                                   (if (= x 0)
                                     {:performance
                                      (->> result
                                           (map #(+ 1 (/ % 100)))
                                           (reduce *)
                                           (#(js/Math.pow % (/ 1 (count result))))
                                           (#(- % 1))
                                           (* 100))}
                                     (let [start-date (time/minus end-date (time/years 1))
                                           performance (:performance (growth-since start-date end-date))]
                                       (recur (dec x)
                                              start-date
                                              (conj result performance)))))))
        years (->> unitized-values
                   (group-by #(time/year (:cljs-date %)))
                   (map (fn [[year v]]
                          (let [v (sort-by :timestamp v)
                                start (first v)
                                end (last v)]
                            [year {:performance (growth-percentage (:unit-price start) (:unit-price end))
                                   :overall (growth-percentage (:value start) (:value end))}])))
                   (into {}))]
    {:all-time (growth-since (->> unitized-values first as-date) todays-date)
     :month (growth-since month todays-date)
     :last-month (growth-since (time/minus month (time/months 1)) month)
     :ytd (growth-since (time/date-time (time/year todays-date)) todays-date)
     :years-1 (growth-years-rolling 1)
     :years-2 (growth-years-rolling 2)
     :years-3 (growth-years-rolling 3)
     :years-4 (growth-years-rolling 4)
     :years-5 (growth-years-rolling 5)
     :years-7 (growth-years-rolling 7)
     :years-10 (growth-years-rolling 10)
     :years years}))

;;;;; QUERIES

(defn birthday [coll]
  (->> coll
       (filter (type-of-f? :date-of-birth))
       first
       (#(or % {:year 1970 :month 1 :day 1}))
       timestamped
       wrap-age))


(defn salaries [coll]
  (->> coll
       (filter (type-of-f? :salary))
       (map timestamped)
       (sort-by :timestamp)))


(defn combined-salaries [coll]
  (let [salaries (salaries coll)]
    (loop [salaries salaries
           existing {}
           result []]
      (if (empty? salaries)
        (sort-by :timestamp result)
        (let [salary (first salaries)
              existing (assoc existing (:owner salary) salary)
              value (->> existing
                         vals
                         (map :value)
                         (reduce +))
              name (->> existing
                        vals
                        (map (fn [{:keys [name owner value]}] (str owner " - " name " - " value)))
                        (string/join " | ")
                        (str value " : "))
              salary (assoc salary :name name :value value)]
          (recur
           (rest salaries)
           existing
           (conj result salary)))))))


(defn monthly-transactions [assets]
  (->> assets
       vals
       (map :transactions)
       (reduce into)
       (map #(assoc % :grouping [(:year %) (:month %)]))
       (group-by :grouping)
       (map (fn [[g t]]
              [g (->> t (map :amount) (reduce +))]))
       (into {})))


(defn income-expense [monthly-transactions coll]
  (->> coll
       (filter (type-of-f? :income-expense))
       (map (fn [{:keys [year month income expense] :as m}]
              (let [savings (get monthly-transactions [year month] 0)]
                (assoc m
                       :day (time/day (time/last-day-of-the-month (time/date-time year month)))
                       :expected-saving-rate (->> (/ expense income) (- 1) (* 100))
                       :recorded-saving-rate (-> savings (/ income) (* 100))
                       :savings savings))))
       (map timestamped)
       (sort-by :timestamp)))


(defn wi-goals [coll {:keys [cljs-date] :as birthday} daily-wi]
  (let [start-wi (first daily-wi)]
    (->> coll
         (filter (type-of-f? :wi-goal))
         (map (fn [{:keys [wealth-index age] :as wi}]
                (let [goal-age (time/plus cljs-date (time/years age))]
                  (assoc wi
                         :name (str wealth-index "@" age)
                         :age age
                         :graph [start-wi
                                 (->> {:wi wealth-index
                                       :cljs-date goal-age}
                                      cljs-timestamped)])))))))


(defn year-goals [coll]
  (->> (filter (type-of-f? :year-goal) coll)
       (map (fn [{:keys [percentage] :as m}]
              (assoc m :name (str percentage "%"))))
       (group-by :year)))



(defn asset [asset-data]
  (let [{:keys [open-asset transaction value close-asset]}
        (group-by :data-type asset-data)
        closed? (not (empty? close-asset))
        details (first open-asset)
        open-transaction (assoc details :amount (:value details))
        close (first close-asset)
        values (->> [open-asset transaction value
                      (when closed?
                        [(assoc close :value 0)])]
                    (keep identity)
                    (reduce into)
                    (map timestamped)
                    (sort-by :timestamp))
        transactions (->> [transaction
                           [open-transaction]
                           (when closed? [(assoc close :amount (- (:value close)))])]
                          (keep identity)
                          (reduce into)
                          (map timestamped)
                          (sort-by :timestamp))
        daily-values (daily-values :value values)
        contribution (contribution-amount transactions)
        growth (growth-amount daily-values)
        value (:value (last daily-values))
        self-growth (- value contribution)
        self-growth-precentage (growth-percentage contribution value)]
    (-> details
        timestamped
        (update :include-in-net #(= yes %))
        (update :is-tfsa #(= yes %))
        (assoc :closed? closed?
               :values values
               :transactions transactions
               :daily-values daily-values
               :growth-all-time (growth-all-time daily-values)
               :growth-year (growth-year daily-values)
               :growth-month (growth-month daily-values)
               :contribution-growth-amount contribution
               :self-growth-amount self-growth
               :self-growth-precentage self-growth-precentage
               :growth-amount growth
               :start-value (:value (first daily-values))
               :value (:value (last daily-values))
               :performance (unitize (:units details) daily-values transactions)))))


(defn assets [coll]
  (->> coll
       (filter (types-of-f? :open-asset :transaction :value :close-asset))
       (group-by :name)
       (map (fn [[k a]]
              [k (asset a)]))
       (into {})))


(defn assets-per-person [assets]
  (->> assets
       vals
       (group-by :owner)
       (map (fn [[owner assets]]
              {:owner owner
               :value (->> assets
                           (map :value)
                           (reduce +))}))
       ))


(defn daily-asset-values [assets]
  (->> assets
       (map :daily-values)
       (reduce into)
       (group-by :timestamp)
       (map (fn [[t v]]
              (->> v
                   (map :value)
                   (reduce + 0)
                   (assoc {:cljs-date (time.coerce/from-long t)}
                          :value))))
       (map cljs-timestamped)
       (sort-by :timestamp)
       ))


(defn daily-wi [birthday daily-salaries daily-asset-values]
  (let [assets (zipmap (map :timestamp daily-asset-values) daily-asset-values)]
    (->> daily-salaries
         (map
          (fn [{:keys [timestamp cljs-date value] :as m}]
            (let [asset-value (get-in assets [timestamp :value])
                  age (time/in-years (time/interval (:cljs-date birthday) cljs-date))]
              (assoc m
                     :age age
                     :salary value
                     :asset-value (or asset-value 0)
                     :wi (wi asset-value value age)
                     )))))))

(defn income-expense-wi [{:keys [asset-value age]} income-expense]
  (let [{:keys [income expense]} (last income-expense)]
    {:income-wi (wi asset-value income age)
     :expense-wi (wi asset-value expense age)}))


(defn asset-groups [assets]
  (->> assets
       vals
       (group-by :asset-type)
       (map
        (fn [[t assets]]
          (let [daily-values (daily-asset-values assets)
                transactions (->> assets
                                  (map :transactions)
                                  (reduce into)
                                  (sort-by :timestamp))
                {:keys [value]} (last daily-values)
                contribution (contribution-amount transactions)
                growth (growth-amount daily-values)
                self-growth (- value contribution)
                self-growth-precentage (growth-percentage contribution value)]
            [t {:asset-type t
                :assets assets
                :daily-values daily-values
                :growth-all-time (growth-all-time daily-values)
                :growth-year (growth-year daily-values)
                :growth-month (growth-month daily-values)
                :contribution-growth-amount contribution
                :growth-amount growth
                :self-growth-amount self-growth
                :self-growth-precentage self-growth-precentage
                :value value
                :transactions transactions}])))
    (into {})))


(defn year-growth-months [start daily-values transactions]
  (let [today (time/today-at-midnight)
        transactions (->> transactions
                         group-by-month
                         (map
                          (fn [[m t]]
                            [m (->> t
                                    (map :amount)
                                    (reduce + 0))]))
                         (into {}))
        monthly-values (->> daily-values
                            (filter (fn [{:keys [cljs-date]}]
                                      (let [midnight (time/at-midnight cljs-date)]
                                        (or (time/equal? midnight (time/last-day-of-the-month cljs-date))
                                            (time/equal? midnight today)))))
                            (map with-month))
        monthly-values (zipmap (map :month monthly-values) monthly-values)]
    (->> monthly-values
         (map
          (fn [[month {:keys [value] :as m}]]
            (let [prev-value (get-in monthly-values [(dec month) :value] start)
                  transaction (get transactions month 0)
                  change (- value prev-value)
                  self-growth-amount (- change transaction)]
              (assoc m
                     :growth-amount change
                     :transacted-amount transaction
                     :self-growth-amount self-growth-amount)))))))


(defn years [assets daily-salaries daily-wi year-goals]
  (let [with-year #(assoc % :year (time/year (:cljs-date %)))
        assets (vals assets)
        salaries (group-by-year daily-salaries)
        wi (group-by-year daily-wi)
        transactions (->> assets
                          (map :transactions)
                          (reduce into)
                          (sort-by :timestamp)
                          group-by-year)
        years-data
        (->> (daily-asset-values assets)
             group-by-year
             (map
              (fn [[y daily-values]]
                (let [transactions (get transactions y [])
                      transaction-total (->> transactions (map :amount) (reduce +))
                      wi-growth (->> (get wi y [])
                                     ((juxt last first))
                                     (map :wi)
                                     (apply -))
                      salary-growth (->> (get salaries y [])
                                         ((juxt last first))
                                         (map :value)
                                         (apply -))
                      growth-year (growth-all-time daily-values)]
                  [y {:year y
                      :wi wi-growth
                      :salary salary-growth
                      :growth-year growth-year
                      :daily-values daily-values
                      :transactions transactions
                      :transaction-total transaction-total
                      :end (:value (last daily-values))}])))
             (into {}))]
    (->> years-data
         (map
          (fn [[y {:keys [daily-values transactions end transaction-total] :as d}]]
            (let [start (get-in years-data [(dec y) :end] 0)
                  total (- end start)
                  no-growth? (= 0 total)
                  transaction-growth-percent (if no-growth? 0 (* 100 (/ transaction-total total)))
                  self-growth-percent (if no-growth? 0 (- 100 transaction-growth-percent))
                  goals (->> (get year-goals y)
                             (map
                              (fn [{:keys [percentage] :as m}]
                                (let [end-g (* start (+ 1 (/ percentage 100)))
                                      expected (- end-g start)]
                                  (assoc m
                                         :start start
                                         :end end-g
                                         :expected-monthly (/ expected 12)
                                         :expected expected)))))]
              [y (assoc d
                        :start start
                        :self-growth-percent self-growth-percent
                        :transaction-growth-percent transaction-growth-percent
                        :growth-months (year-growth-months start daily-values transactions)
                        :goals goals)])))
         (into {}))))


(defn monthly-interest [annual-percentage]
  (-> annual-percentage
      (/ 12 100)
      (+ 1)))


(defn money-lifetime [{:keys [salary asset-value]} {:keys [inflation percent-of-salary asset-growth] :as life}]
  (let [deduction            (* (/ salary 100) percent-of-salary)
        monthly-inflation    (monthly-interest inflation)
        monthly-asset-growth (monthly-interest asset-growth)
        fifty-year-months    (* 12 50)]
    (loop [deduction deduction
           value     asset-value
           months    0]
      (if (or (> 0 (- value deduction)) (> months fifty-year-months))
        (assoc life :months (mod months 12) :years (int (/ months 12)) :value value :monthly-salary deduction)
        (let [d (* deduction monthly-inflation)
              v (* (- value deduction) monthly-asset-growth)
              m (inc months)]
          (recur d v m))))))


(defn money-lifetimes [current-values coll]
  (->> coll
       (filter (type-of-f? :money-lifetime))
       (map #(money-lifetime current-values %))))


(defn tfsa-tracking [assets]
  (->> assets
       vals
       (filter #(true? (:is-tfsa %)))
       (group-by :owner)
       (map (fn [[owner assets]]
              (let [transactions (->> assets
                                      (map :transactions)
                                      (reduce into))
                    total (->> transactions
                               (map :amount)
                               (reduce +))
                    yearly (->> transactions
                                (map (fn [{:keys [cljs-date amount]}]
                                       [(-> cljs-date (time/minus (time/months 2)) time/year)
                                        amount]))
                                (group-by first)
                                (map (fn [[year v]]
                                       [year (->> v (map last) (reduce +))]))
                                (into {}))]
                [owner {:owner owner
                        :lifetime total
                        :yearly yearly}])))
       (into {})))


(def lookback-in-months 12)
(def assumed-return-after-inflation 5)
(def assumed-safe-withdrawal-rate 4)

(defn years-till-financially-independent
  "Implements the spreadsheet found here
  https://www.mrmoneymustache.com/2012/01/13/the-shockingly-simple-math-behind-early-retirement/"
  ([net-worth take-home-pay saving-rate]
   (years-till-financially-independent
    net-worth take-home-pay assumed-return-after-inflation saving-rate assumed-safe-withdrawal-rate))
  ([net-worth take-home-pay return-after-inflation saving-rate withdrawal-rate]
   (let [years-of-take-home-pay (/ net-worth (* take-home-pay 12))
         return-after-inflation (/ return-after-inflation 100)
         saving-rate (/ saving-rate 100)]
     (loop
         [year 0
          investment-gains 0
          years-of-take-home-income years-of-take-home-pay
          stash-relative-to-spending 0
          withdraw-spend-percent 0]
       (if (or (>= year 1000) (>= withdraw-spend-percent 100))
         year
         (let [year' (inc year)
               investment-gains'
               (+
                (-
                 (*
                  years-of-take-home-income
                  (+
                   1
                   return-after-inflation))
                 years-of-take-home-income)
                (*
                 (/
                  saving-rate
                  2)
                 return-after-inflation))
               years-of-take-home-income' (+ (+ years-of-take-home-income investment-gains') saving-rate)
               stash-relative-to-spending' (/ years-of-take-home-income' (- 1 saving-rate))
               withdraw-spend-percent' (* stash-relative-to-spending' withdrawal-rate)
               ]
           (recur
            year' investment-gains' years-of-take-home-income'
            stash-relative-to-spending' withdraw-spend-percent')))))))


(defn money-health [{:keys [asset-value age]} asset-groups income-expense]
  (let [multiplier 300
        em-fund-value (get-in asset-groups ["Emergency Fund" :value] 0)
        income-expense (take-last lookback-in-months income-expense)
        ie-count       (-> (count income-expense)
                           (min lookback-in-months)
                           (max 1))
        total-expense (->> income-expense
                         (map :expense)
                         (reduce +))
        avg-expense   (/ total-expense ie-count)
        total-income (->> income-expense
                          (map :income)
                          (reduce +))
        avg-income   (/ total-income ie-count)
        available-to-save (max 0 (- avg-income avg-expense))
        em-ratio  (if (> avg-expense 0)
                    (/ em-fund-value avg-expense)
                    0)
        four-percent-rule-total (* avg-expense multiplier)
        percent-of-four-completed (if (> four-percent-rule-total 0)
                                    (* 100 (/ asset-value four-percent-rule-total))
                                    0)
        four-percent-rule-over-time (->> income-expense
                                         (map (fn [{:keys [expense] :as m}]
                                                (assoc m :four-percent-rule-total
                                                       (* expense multiplier)))))
        total-saving-rate (->> income-expense (map :recorded-saving-rate) (reduce +))
        avg-saving-rate (/ total-saving-rate ie-count)
        current-take-home-pay (-> income-expense last :income)
        years-till-financially-independent' (years-till-financially-independent asset-value current-take-home-pay avg-saving-rate)
        age-when-financially-independent (+ age years-till-financially-independent')
        retire-years-fixing-saving-rate (->> [10 20 30 40 50 60]
                                             (map #(hash-map
                                                    :saving-rate %
                                                    :years (years-till-financially-independent
                                                            asset-value
                                                            current-take-home-pay
                                                            %)))
                                             (into [{:saving-rate avg-saving-rate
                                                     :years years-till-financially-independent'
                                                     :current true}])
                                             (sort-by :saving-rate)
                                             reverse)]
    {:avg-monthly-available-to-save available-to-save
     :avg-monthly-expense (js/Math.round avg-expense)
     :emergency-fund-ratio em-ratio
     :four-percent-rule-total four-percent-rule-total
     :percent-of-four-completed percent-of-four-completed
     :four-percent-rule-over-time four-percent-rule-over-time
     :avg-saving-rate avg-saving-rate
     :years-till-financially-independent years-till-financially-independent'
     :age-when-financially-independent age-when-financially-independent
     :retire-years-fixing-saving-rate retire-years-fixing-saving-rate}))


(defn independence-years-tracking [income-expense daily-wi]
  (let [monthly-net-worth
        (->> daily-wi
             (group-by (fn [{:keys [date] :as wi}]
                         [(.getFullYear date) (inc (.getMonth date))]))
             (map
              (fn [[d v]]
                [d (->> v
                        (sort-by :timestamp)
                        last
                        :asset-value)]))
             (into {}))
        income-expense-years-to-independent
        (let [look-window (/ lookback-in-months 2)]
          (loop [given income-expense
                 result []]
            (if (empty? given)
              (->> result (sort-by :timestamp))
              (let [{:keys [income year month] :as month-given} (first given)
                    forward-window (take look-window given)
                    backward-window (take look-window result)
                    given-window (concat forward-window backward-window)
                    avg-savings-rate (/ (->> given-window (map :recorded-saving-rate) (reduce +))
                                        (count given-window))
                    net-worth (get monthly-net-worth [year month] 0)]
                (recur
                 (rest given)
                 (conj result (assoc month-given :years-to-independence
                                     (years-till-financially-independent net-worth income avg-savings-rate))))))))]
    income-expense-years-to-independent))


(defn all-your-bucks [coll]
  (let [year-goals (year-goals coll)
        birthday (birthday coll)
        salaries (combined-salaries coll)
        daily-salaries (daily-values :value salaries)
        assets (assets coll)
        net-assets (->> assets
                        (filter (fn [[_ v]] (:include-in-net v))))
        daily-asset-values (daily-asset-values (vals net-assets))
        daily-wi (daily-wi birthday daily-salaries daily-asset-values)
        current-wi (last daily-wi)
        monthly-transactions (monthly-transactions net-assets)
        income-expense (income-expense monthly-transactions coll)
        income-expense-wi (income-expense-wi current-wi income-expense)
        current-values (-> current-wi
                           (assoc :growth-year (growth-year daily-asset-values)
                                  :growth-month (growth-month daily-asset-values))
                           (merge income-expense-wi))
        asset-groups (asset-groups net-assets)
        assets-per-person (assets-per-person net-assets)
        years (years net-assets daily-salaries daily-wi year-goals)
        wi-goals (wi-goals coll birthday daily-wi)
        money-lifetimes (money-lifetimes current-values coll)
        tfsa-tracking (tfsa-tracking assets)
        money-health (money-health current-wi asset-groups income-expense)
        independence-years-tracking (independence-years-tracking income-expense daily-wi)
        ]
    {:birthday birthday
     :salaries salaries
     :income-expense income-expense
     :assets assets
     :assets-per-person assets-per-person
     :daily-wi daily-wi
     :asset-groups asset-groups
     :years years
     :wi-goals wi-goals
     :money-lifetimes money-lifetimes
     :current-values current-values
     :tfsa-tracking tfsa-tracking
     :money-health money-health
     :monthly-transactions monthly-transactions
     :independence-years-tracking independence-years-tracking}))
