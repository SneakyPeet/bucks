(ns bucks.queries)

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
