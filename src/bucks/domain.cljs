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
    "A growth percentage goal for a given year"]])


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


(defn wi-goals [coll]
  (->> coll
       (filter (type-of-f? :wi-goal))))


(defn year-goals [coll]
  (->> coll
       (filter (type-of-f? :year-goal))
       (sort-by :year)
       reverse))


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