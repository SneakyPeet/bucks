(ns bucks.domain
  (:require [cljs.spec.alpha :as s]
            [cljs-time.core :as time]
            [cljs-time.coerce :as time.coerce]))


;;;; DEFAULTS

(def asset-types #{"TFSA" "RA" "Crypto" "Savings" "CFD" "ETF" "UnitTrust"})
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

(s/def :d/exclude-from-net #(contains? #{yes no} %))

(s/def :d/inflation pos?)

(s/def :d/percent-of-salary pos?)

(s/def :d/asset-growth number?)


;;;; DATA TYPES

(def data-types-config
  [["date-of-birth"
    [:d/year :d/month :d/day]
    "Your date of birth to use in wealth index calculations."]
   ["salary"
    [:d/name :d/year :d/month :d/day :d/value]
    "A Salary Change. Name is the Name Of Employer. Value the Monthly Salary Before Tax."]
   ["open-asset"
    [:d/name :d/year :d/month :d/day :d/asset-type :d/value :d/units :d/exclude-from-net]
    (str "A new Asset. Name is the name of the asset. Asset-type should be one of the following " asset-types
         ". Requires opening value and units. Exclude from Net indicates if an asset should be excluded for the wealth index and total asset value calculations (can be y or n). Asset value is defaulted to 0 and an initial transaction can be added using 'transaction'.")]
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
    [:d/year :d/name :d/percentage]
    "A growth percentage goal for a given year"]
   ["money-lifetime"
    [:d/inflation :d/percent-of-salary :d/asset-growth]
    "Calculates the lifetime of your money given annual inflation and asset growth compounded monthly with monthly withdrawals of a percentage of your salary."]])


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


(defn wrap-age [{:keys [timestamp] :as birthday}]
  (assoc birthday
         :age (time/in-years (time/interval
                              (time.coerce/from-long timestamp)
                              (time/now)))))


(defn monthly-values
  "Calculates monthly values from first item to now.
  Key is the map value containing the values"
  [k coll]
  (let [sorted-coll (sort-by :timestamp coll)
        now         (time/now)
        start       (first sorted-coll)]
    (if (empty? sorted-coll)
      []
      (loop [month          (time/first-day-of-the-month (:cljs-date start))
             value          (get start k)
             remaining-coll (rest sorted-coll)
             result         []]
        (let [future? (time/after? month now)
              next (first remaining-coll)
              new? (and (not (empty? remaining-coll))
                        (time/after? month (:cljs-date next)))
              next-month (time/plus month (time/months 1))
              next-value (if new? (get next k) value)
              next-result (conj result {:cljs-date month :value next-value})
              next-changes (if new? (drop 1 remaining-coll) remaining-coll)]
          (if-not future?
            (recur next-month next-value next-changes next-result)
            (map cljs-timestamped result)))))))


(defn wi [value salary age] (/ (/ value (* 12 salary)) (/ age 10)))


(defn growth-percentage
  ([] 0)
  ([x] 0)
  ([start end]
  (if (and (number? start) (number? end) (not= 0 end) (not= 0 start))
    (* 100 (- (/ end start) 1))
    0)))


(defn growth-all-time [month-values]
  (->> ((juxt first last) month-values)
       (map :value)
       (apply growth-percentage)))


(defn growth-year [month-values]
  (let [year (-> (time/now)
                 time/year
                 (time/date-time)
                 (time/minus (time/millis 1)))]
    (->> month-values
         (filter #(time/after? (:cljs-date %) year))
         growth-all-time)))


(defn growth-month [month-values]
  (->> month-values
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


(defn wi-goals [coll {:keys [cljs-date] :as birthday} monthly-wi]
  (let [start-wi (first monthly-wi)]
    (->> coll
         (filter (type-of-f? :wi-goal))
         (map (fn [{:keys [wealth-index age] :as wi}]
                (let [goal-age (time/plus cljs-date (time/years age))]
                  (assoc wi
                         :name (str age "@" wealth-index)
                         :age age
                         :graph [start-wi
                                 (->> {:wi wealth-index
                                       :cljs-date goal-age}
                                      cljs-timestamped)])))))))


(defn year-goals [coll]
  (->> (filter (type-of-f? :year-goal) coll)
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
        monthly-values (monthly-values :value values)
        contribution (contribution-amount transaction)
        growth (growth-amount monthly-values)
        self-growth (- growth contribution)]
    (-> details
        timestamped
        (update :exclude-from-net #(= yes %))
        (assoc :closed? closed?
               :values values
               :transactions transactions
               :monthly-values monthly-values
               :growth-all-time (growth-all-time monthly-values)
               :growth-year (growth-year monthly-values)
               :growth-month (growth-month monthly-values)
               :contribution-growth-amount contribution
               :self-growth-amount self-growth
               :growth-amount growth
               :start-value (:value (first monthly-values))
               :value (:value (last monthly-values))))))


(defn assets [coll]
  (->> coll
       (filter (types-of-f? :open-asset :transaction :value :close-asset))
       (group-by :name)
       (map (fn [[k a]]
              [k (asset a)]))
       (into {})))


(defn monthly-asset-values [assets]
  (->> assets
       (map :monthly-values)
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


(defn monthly-wi [birthday monthly-salaries monthly-asset-values]
  (let [assets (zipmap (map :timestamp monthly-asset-values) monthly-asset-values)]
    (->> monthly-salaries
         (map
          (fn [{:keys [timestamp cljs-date value] :as m}]
            (let [asset-value (get-in assets [timestamp :value])
                  age (time/in-years (time/interval (:cljs-date birthday) cljs-date))]
              (assoc m
                     :age age
                     :salary value
                     :asset-value asset-value
                     :wi (wi asset-value value age)
                     )))))))


(defn asset-groups [assets]
  (->> assets
       vals
       (group-by :asset-type)
       (map
        (fn [[t assets]]
          (let [monthly-values (monthly-asset-values assets)
                transactions (->> assets
                                  (map :transactions)
                                  (reduce into)
                                  (sort-by :timestamp))
                {:keys [value]} (last monthly-values)
                contribution (contribution-amount transactions)
                growth (growth-amount monthly-values)
                self-growth (- growth contribution)]
            [t {:asset-type t
                :monthly-values monthly-values
                :growth-all-time (growth-all-time monthly-values)
                :growth-year (growth-year monthly-values)
                :growth-month (growth-month monthly-values)
                :contribution-growth-amount contribution
                :growth-amount growth
                :self-growth-amount self-growth
                :value value
                :transactions transactions}])))
    (into {})))


(defn year-growth-months [start monthly-values transactions]
  (let [transactions (->> transactions
                         group-by-month
                         (map
                          (fn [[m t]]
                            [m (->> t
                                    (map :amount)
                                    (reduce + 0))]))
                         (into {}))
        monthly-values (map with-month monthly-values)
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


(defn years [assets monthly-salaries monthly-wi year-goals]
  (let [with-year #(assoc % :year (time/year (:cljs-date %)))
        assets (vals assets)
        salaries (group-by-year monthly-salaries)
        wi (group-by-year monthly-wi)
        transactions (->> assets
                          (map :transactions)
                          (reduce into)
                          (sort-by :timestamp)
                          group-by-year)
        years-data
        (->> (monthly-asset-values assets)
             group-by-year
             (map
              (fn [[y monthly-values]]
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
                      growth-year (growth-all-time monthly-values)]
                  [y {:year y
                      :wi wi-growth
                      :salary salary-growth
                      :growth-year growth-year
                      :monthly-values monthly-values
                      :transactions transactions
                      :transaction-total transaction-total
                      :end (:value (last monthly-values))}])))
             (into {}))]
    (->> years-data
         (map
          (fn [[y {:keys [monthly-values transactions end transaction-total] :as d}]]
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
                        :growth-months (year-growth-months start monthly-values transactions)
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


(defn all-your-bucks [coll]
  (let [year-goals (year-goals coll)
        birthday (birthday coll)
        salaries (salaries coll)
        monthly-salaries (monthly-values :value salaries)
        assets (assets coll)
        open-assets (->> assets
                         (filter (fn [[_ v]] (false? (:closed? v)))))
        monthly-asset-values (monthly-asset-values (vals open-assets))
        monthly-wi (monthly-wi birthday monthly-salaries monthly-asset-values)
        current-values (-> (last monthly-wi)
                           (assoc :growth-year (growth-year monthly-asset-values)
                                  :growth-month (growth-month monthly-asset-values)))
        asset-groups (asset-groups assets)
        years (years assets monthly-salaries monthly-wi year-goals)
        wi-goals (wi-goals coll birthday monthly-wi)
        money-lifetimes (money-lifetimes current-values coll)
        ]
    {:birthday birthday
     :salaries salaries
     :assets assets
     :monthly-wi monthly-wi
     :asset-groups asset-groups
     :years years
     :wi-goals wi-goals
     :money-lifetimes money-lifetimes
     :current-values current-values}))
