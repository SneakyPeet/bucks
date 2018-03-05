(ns bucks.example
  (:require [cljs-time.core :as time]
            [clojure.string :as str]))

(def now (time/now))

(def this-year (time/year now))

(def last-year (dec this-year))

(def dob [["date-of-birth" 1984 12 6]])

(def wi [["wi-goal" 2 40]
         ["wi-goal" 5 65]])

(def money-lifetime [["money-lifetime" 5 100 10]
                     ["money-lifetime" 7.5 75 5]])

(def year-goals
  [["year-goal" this-year 26]
   ["year-goal" this-year 15]
   ["year-goal" last-year 20]])

(def salaries
  [["salary" "Company 1" (dec last-year) 1 2 25000]
   ["salary" "Company 1" last-year 6 1 27000]
   ["salary" "Company 1" this-year 1 1 36000]])

(def asset1
  [["open-asset" "Asset 1" (dec last-year) 12 30 "RA" 20000 10 "y"]
   ["transaction" "Asset 1" last-year 3 1 1000 19800 1]
   ["transaction" "Asset 1" last-year 6 1 2000 23700 2]
   ["transaction" "Asset 1" last-year 8 1 -1000 23300 -1]
   ["transaction" "Asset 1" last-year 12 1 2000 25700 2]
   ["transaction" "Asset 1" this-year 1 1 -1500 24300 -1.5]])

(def asset2
  [["open-asset" "Asset 2" last-year 5 15 "TFSA" 45000 0 "y"]
   ["value" "Asset 2" last-year 6 30 46000]
   ["value" "Asset 2" last-year 7 31 46500]
   ["value" "Asset 2" last-year 8 31 45500]
   ["value" "Asset 2" last-year 9 30 47000]
   ["value" "Asset 2" last-year 10 31 47800]
   ["value" "Asset 2" last-year 11 30 47500]
   ["value" "Asset 2" last-year 12 31 47900]
   ["value" "Asset 2" this-year 1 31 48800]])

(def closed-asset
  [["open-asset" "Closed Asset 1" last-year 2 2 "Crypto" 100000 1 "y"]
   ["transaction" "Closed Asset 1" last-year 6 6 1000 120000 0.03]
   ["close-asset" "Closed Asset 1" last-year 9 9 130000]])

(def ignore-asset
  [["open-asset" "Ignored Asset 1" last-year 3 2 "Crypto" 200000 1 "n"]])

(def test-piped-csv
  (->> [dob
        wi
        year-goals
        money-lifetime
        salaries
        asset1
        asset2
        closed-asset
        ignore-asset]
       (reduce into)
       (map #(->> % (map str) (str/join "|")))
       (str/join "\n")))
