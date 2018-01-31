(ns bucks.queries
  (:require [java-time :as t]))


(defn with-date [{:keys [timestamp] :as m}]
  (assoc m :date (t/local-date-time (t/instant timestamp) "UTC")))

(defn year-month [m]
  (t/as (:date m) :year :month-of-year))


(defmulti query (fn [type state params] type))

(defmethod query :salaries [_ state _] (:salaries state))


(defmethod query :assets [_ state _]
  (->> (:assets state)
       vals
       (map #(dissoc % :transactions :asset-values))))


(defmethod query :asset-history [_ state {:keys [name]}]
  (->> (get-in state [:assets name])
       (#(concat (:transactions %)
                 (->> (:asset-values %) (map (fn [{:keys [value] :as a}]
                                               (assoc a
                                                      :transaction-type :value
                                                      :amount value
                                                      :units 0))))))
       (sort-by :timestamp)))

(defmethod query :goals [_ state _] (->> (:yearly-goals state)
                                         vals
                                         (map vals)
                                         (reduce into)))

(defmethod query :wi-goals [_ state _] (vals (:wealth-index-goals state)))

;;;; REPORT


(defn monthly-values [changes]
  (let [changes (sort-by :date changes)
        now (t/local-date (t/instant) "UTC")]
    (if (empty? changes)
      []
      (loop [month (:date (first changes))
             value (:value (first changes))
             upcoming-changes (rest changes)
             result []]
        (let [future? (t/after? month now)
              new? (and (not-empty upcoming-changes)
                        (t/after? month (:date (first upcoming-changes))))
              next-month (t/plus month (t/months 1))
              next-value (if new? (:value (first upcoming-changes)) value)
              next-result (conj result {:date month :value next-value})
              next-changes (if new? (drop 1 upcoming-changes) upcoming-changes)]
          (if future?
            result
            (recur next-month next-value next-changes next-result)))))))


(defn with-last-monthy-values [values]
  (->> values
       (map with-date)
       (group-by year-month)
       (map (fn [[k v]]
              {:date (apply t/local-date k)
               :value (->> v (sort-by :timestamp) last :value)}))
       monthly-values))


(defn salary-month-values [state]
  (->> state :salaries with-last-monthy-values))


(defn asset-month-values [{:keys [asset-values]}]
  (with-last-monthy-values asset-values))


(defn net-asset-month-values [state]
  (->> state
       :assets
       vals
       (map asset-month-values)
       (reduce into)
       (group-by :date)
       (map (fn [[m v]]
              {:date m
               :value (->> v
                           (map :value)
                           (reduce + 0))}))
       (sort-by :date)))


(defn wi [net salary age] (/ (/ net salary) (/ age 10)))


(defn monthly-wealth-index [state]
  (let [dob (t/local-date (t/instant (:date-of-birth state)) "UTC")
        salary-months (salary-month-values state)
        assets (->> (net-asset-month-values state)
                    (map (juxt :date :value))
                    (into {}))]
    (->> salary-months
         (map (fn [{:keys [date value]}]
                (let [net (get assets date 0)
                      age (t/time-between dob date :years)]
                  {:date date
                   :net net
                   :age age
                   :salary value
                   :wi (wi net value age)}))))))


(defn monthly-wi-goals [state]
  (let [index (monthly-wealth-index state)
        dob (t/local-date (t/instant(:date-of-birth state)) "UTC")
        start (first index)
        date (get start :date (t/local-date))
        value (get start :wi 0)
        goals (->> state
                   :wealth-index-goals
                   vals
                   (map
                    (fn [{:keys [age units]}]
                      (let [goal-date (t/plus dob (t/years age))
                            months (t/time-between dob goal-date :months)
                            steps (/ (- units value) months)]
                        (fn [i] (* i steps))))))]
    (->> index
         (map-indexed
          (fn [i {:keys [wi] :as v}]
            (assoc v :goals
                   (conj (map #(% i) goals) wi)))))))


(defn unwrap-date [{:keys [date] :as m}]
  (-> m
      (assoc :timestamp (t/to-millis-from-epoch (t/offset-date-time date 0)))
      (dissoc :date)))


(defn unwrap-dates [coll] (map unwrap-date coll))


(defn prep-report-data [state]
  (assoc state
         :wi (unwrap-dates (monthly-wi-goals state))))
