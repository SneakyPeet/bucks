(ns bucks.parse
  (:require [bucks.domain :as d]
            [cljs.reader :as reader]
            [cljs.spec.alpha :as s]
            [clojure.string :as str]
            [testdouble.cljs.csv :as csv]))


(def actions
  {"salary" [:d/year :d/month :d/day :d/name :d/amount]})


;;;; PARSE

(defn- parse-symbol [s]
  (if (symbol? s)
    (str s)
    s))


(defn- parse-row [row]
  (map
   (fn [field]
     (-> field
         str/trim
         reader/read-string
         parse-symbol))
   row))


(defn- not-empty-row? [row]
  (->> row
       (filter (comp not empty?))
       count
       (not= 0)))


(defn- validate-row-data [key data]
  (let [specs (get actions key)]
    (->> specs
         (map-indexed
          (fn [i spec]
            (let [field (nth data i)]
              {:valid? (s/valid? spec field)
               :spec   spec
               :field  field}))))))


(defn- validate-row [[key & data]]
  (if-not (contains? actions key)
    {:valid? false :error (str key " is an invalid action") :data data}
    (let [validation (validate-row-data key data)
          specs (->> (get actions key))]
      (if (empty? (->> validation (map :valid?) (filter false?)))
        {:valid? true :specs specs :data data :key (keyword key)}
        (let [failed-specs (->> validation
                                (filter (comp false? :valid?))
                                (map :spec))]
          {:valid false :data data
           :specs specs
           :key (keyword key)
           :failed-specs failed-specs
           :error (str key " has invalid values for "
                       (map name failed-specs)
                       ". Expected " (map name specs))})))))


(defn- as-domain-value [{:keys [specs key data]}]
  (-> (zipmap
       (map (comp keyword name) specs)
       (take (count specs) data))
      (assoc :action-type key)))


(defn parse [separator data]
  (->> (csv/read-csv data :separator separator)
       (filter not-empty-row?)
       (map (comp validate-row parse-row))))


(defn as-domain-values [rows]
  (->> rows
       (filter :valid?)
       (map as-domain-value)))


(defn un-parse [separator coll]
  (->> coll
       (map
        (fn [{:keys [action-type] :as m}]
          (let [fields (->> (name action-type)
                            (get actions)
                            (map (comp keyword name)))]
            (->> fields
                 (map #(get m %))))))
       (#(csv/write-csv % :separator separator))))

;;;; tests


(def separator-d "|")
(def test-d
  "salary|2014|2|31|My Company|29450\n
   salary|2014|7|2|My Company|30950\n
   salary|2015|1|1|Some Other Company|36000\n")

(->> (parse separator-d test-d)
     as-domain-values
     (un-parse separator-d))
