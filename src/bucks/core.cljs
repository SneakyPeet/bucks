(ns bucks.core
  (:require [bucks.domain :as domain]
            [bucks.example :as example]
            [bucks.parse :as parse]))

(->> example/test-piped-csv
     (parse/parse parse/pipe)
     parse/as-domain-values
     domain/all-your-bucks
     ;domain/salaries
     ;(domain/monthly-values :value)
     )
