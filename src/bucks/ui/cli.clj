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


(defn get-dispatch [dispatch-command]
  (fn [f & cli-options]
    (fn [& args]
      (let [{:keys [options errors] :as a} (cli/parse-opts args cli-options)]
        (pprint/pprint a)
        (if (not-empty errors)
          (pprint/pprint errors)
          (try+
           (dispatch-command (f options))
           (catch [:type :validation-error] {:keys [error]}
             (pprint/pprint error))
           (catch [:type :app-error] {:keys [error]}
             (pprint/pprint "AN APP ERROR HAS OCCURED")
             (pprint/pprint error))))))))


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
    (println "bucks=>")
    (loop [args-str (read-line)]
      (let [{:keys [f args]} (parse-input args-str)
            f (get fn-map f default)]
        (f args)
        (recur (read-line))))))
