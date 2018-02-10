(ns bucks.domain
  (:require [cljs.spec.alpha :as s]))


;;;; PREDICATES

(defn not-empty-string? [s] (and (string? s)
                                 (not-empty s)))


(defn gt=zero? [n] (and (number? n) (not (neg? n))))


(defn gt-zero? [n] (and (number? n) (pos? n)))


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

(s/def :d/amount gt=zero?)
