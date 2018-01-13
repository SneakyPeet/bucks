(ns bucks.ui.cli
  (:require [bucks.commands :as c]
            [clojure.pprint :as pprint]
            [clojure.tools.cli :as cli]
            [clojure.string :as string]
            [slingshot.slingshot :refer [try+]]
            [bucks.utils :as u]))


(def source-opt
  ["-s" "--source SOURCE" "Source name"
   :default ""
   :validate [#(u/not-empty-string %) "Source name required"]])


(def date-opt
  ["-d" "--date DATE" "Date"
   :default ""
   :validate [u/not-empty-string "Date required"]])


(def salary-amount-opt
  ["-a" "--salary-amount SALARY-AMOUNT" "Monthly Salary (Before Tax)"
   :default 0
   :parse-fn #(Integer/parseInt %)])


(defn pre [] (print "bucks> ") (flush))

(defn print-err [err]
  (cond
    (string? err) (println err)
    (coll? err) (doseq [e err] (println e))
    :else err))

(defn get-dispatch [dispatch-command]
  (fn [f & cli-options]
    (fn [args]
      (let [{:keys [options errors] :as a} (cli/parse-opts args cli-options)]
        (if (not-empty errors)
          (print-err errors)
          (try+
           (dispatch-command (f options))
           (catch [:type :validation-error] {:keys [error]}
             (print-err error))
           (catch [:type :app-error] {:keys [error]}
             (print-err "AN APP ERROR HAS OCCURED")
             (print-err error))))))))


(def cli-config
  {:salary [c/change-salary source-opt date-opt salary-amount-opt]})


(defn parse-input [s]
  (let [args (string/split s #" ")]
    {:f (keyword (first args))
     :args (rest args)}))


(defn default [& args]
  (pprint/pprint "Invalid Action"))


(defn startup [dispatch-command]
  (let [dispatch (get-dispatch dispatch-command)
        fn-map (->> cli-config
                    (map (fn [[k v]]
                           [k (apply dispatch v)]))
                    (into {}))]
    (pre)
    (loop [args-str (read-line)]
      (let [{:keys [f args]} (parse-input args-str)
            f (get fn-map f default)]
        (f args)
        (pre)
        (recur (read-line))))))
