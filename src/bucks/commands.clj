(ns bucks.commands
  (:require [bucks.domain :refer :all]
            [bucks.core :refer :all]))


(defn change-salary [{:keys [source date salary-amount] :as c}]
  (guard :d/salary c)
  (->ChangeSalary source date salary-amount))
