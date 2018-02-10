(ns bucks.parse
  (:require [bucks.domain :as d]
            [cljs.reader :as reader]
            [cljs.spec.alpha :as s]
            [clojure.string :as str]
            [testdouble.cljs.csv :as csv]))


(def actions
  {"salary" [:d/year :d/month :d/day :d/name :d/amount]})


;;;; PARSE

(defn parse-symbol [s]
  (if (symbol? s)
    (str s)
    s))


(defn parse-row [row]
  (map
   (fn [field]
     (-> field
         str/trim
         reader/read-string
         parse-symbol))
   row))


(defn not-empty-row? [row]
  (->> row
       (filter (comp not empty?))
       count
       (not= 0)))


(defn validate-row-data [key data]
  (let [specs (get actions key)]
    (->> specs
         (map-indexed
          (fn [i spec]
            (let [field (nth data i)]
              {:valid? (s/valid? spec field)
               :spec   spec
               :field  field}))))))


(defn validate-row [[key & data]]
  (if-not (contains? actions key)
    {:valid? false :error (str key " is an invalid action") :data data}
    (let [validation (validate-row-data key data)
          specs (->> (get actions key))]
      (if (empty? (->> validation (map :valid?) (filter false?)))
        {:valid? true :specs specs :data data}
        (let [failed-specs (->> validation
                                (filter (comp false? :valid?))
                                (map :spec))]
          {:valid false :data data
           :specs specs
           :failed-specs failed-specs
           :error (str key " has invalid values for "
                       (map name failed-specs)
                       ". Expected " (map name specs))})))))


(defn read [separator data]
  (->> (csv/read-csv data :separator separator)
       (filter not-empty-row?)
       (map (comp validate-row parse-row))
       ))


;;;; tests


(def separator-d "|")
(def test-d
  "saary|2014|2|31|My Company|29450\n
   salary|2014|7|n|My Company|30950\n
   salary|2015|1|1|Some Other Company|36000\n")

;(read separator-d test-d)
