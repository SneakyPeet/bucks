(ns bucks.commands
  (:require [bucks.domain :refer :all]))


(defn change-salary [{:keys [source date salary-amount] :as c}]
  (guard ::salary c)
  (->ChangeSalary source date salary-amount))
