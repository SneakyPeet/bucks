(ns bucks.parse
  (:require [bucks.domain :as d]
            [cljs.reader :as reader]
            [cljs.spec.alpha :as s]
            [clojure.string :as str]
            [testdouble.cljs.csv :as csv]))


(def pipe "|")
(def comma ",")
(def semicolon ";")
(def seperators #{pipe comma semicolon})


(defn- parse-field
  [s]
  (let [value (reader/read-string s)]
    (if (symbol? value) s value)))


(defn- parse-row [row]
  (map
   (fn [field]
     (-> field
         str/trim
         parse-field))
   row))


(defn- not-empty-row? [row]
  (->> row
       (filter (comp not empty?))
       count
       (not= 0)))


(defn- validate-row-data [key data]
  (let [specs (get d/data-types key)]
    (->> specs
         (map-indexed
          (fn [i spec]
            (let [field (nth data i)]
              {:valid? (s/valid? spec field)
               :spec   spec
               :field  field}))))))


(defn- validate-row [[key & data]]
  (if-not (contains? d/data-types key)
    {:valid? false :error (str key " is an invalid data type") :data data}
    (let [validation (validate-row-data key data)
          specs (->> (get d/data-types key))]
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
      (assoc :data-type key)))


(defn parse
  "Parses csv into a collection of validated data rows"
  [separator data]
  (->> (csv/read-csv data :separator separator)
       (filter not-empty-row?)
       (map-indexed (fn [i r]
                      (-> r parse-row validate-row (assoc :i (inc i)))))))


(defn as-domain-values
  "Converts a collection of parsed and valid data rows into domain data."
  [rows]
  (->> rows
       (filter :valid?)
       (map as-domain-value)))


(defn un-parse
  "Converts a collection of domain data into a csv."
  [separator coll]
  (->> coll
       (map
        (fn [{:keys [data-type] :as m}]
          (let [fields (->> (name data-type)
                            (get d/data-types)
                            (map (comp keyword name)))]
            (->> fields
                 (map #(get m %))
                 (cons (name data-type))))))
       (#(csv/write-csv % :separator separator))))
