(ns bucks.utils)

(defn not-empty-string [s] (and (string? s)
                                (not-empty s)))

(defn gt-zero? [n] (and (number? n) (pos? n)))
