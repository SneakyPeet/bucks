(ns bucks.ui.cli
  (:require [bucks.commands :as c]
            [bucks.utils :as u]
            [clojure.pprint :as pprint]
            [clojure.string :as string]
            [commandline.core :as cli-a]
            [slingshot.slingshot :refer [try+]]))

(defn pre [] (print "bucks> ") (flush))


(defn print-err [err]
  (cond
    (string? err) (println err)
    (coll? err) (doseq [e err] (println e))
    :else err))


(defn parse-input [s]
  (let [args (filter not-empty (string/split s #"\s+"))]
    {:f-key (first args)
     :args (rest args)}))

(defn fn-map [dispatch-command]
  (let [dispatch (fn [command] (fn [o] (dispatch-command (command o))))]
    (->>
     [["salary"
       (dispatch c/change-salary)
       [["s" "source" "Salary Source" :string "S" true]
        ["d" "start-date" "Salary Start Date" :time "D"  true]
        ["sa" "salary-amount" "Salary Amount" :float "SA" true]]]]
     (map (fn [[a b c]] [a [a b (conj c help-opt)]]))
     (into {}))))


(defn commandline [[name f options-spec] args]
  (let [options-spec (if)
        options-map (mapv cli-a/option-map options-spec)
        [options _] (cli-a/parse-commandline options-map args)]
    (if (contains? options :h)
      (prn "HELP")
      (f options))))


;;todo
;; help (should remove required field)
;; parsing functions should be done better

(defn startup [dispatch-command]
  (let [dispatch (fn [command] (fn [o] (dispatch-command (command o))))
        fn-map   (fn-map dispatch-command)]
    (do
      (pre)
      (loop [args-string (read-line)]
        (let [{:keys [f-key args]} (parse-input args-string)
              f (get fn-map f-key)]
          (if (nil? f)
            (print-err ["Type 'help' to see a list of available functions."
                        "Type '<function-name> -h' will show a list of options"])
            (try+
             (commandline f args)
             (catch [:type :validation-error] {:keys [error]}
               (print-err error))
             (catch [:type :app-error] {:keys [error]}
               (print-err "AN APP ERROR HAS OCCURED")
               (print-err error))
             (catch org.apache.commons.cli.MissingOptionException e
               (print-err [(.getMessage e)
                           (str "for help type '" f-key " -h'")]))
             (catch org.apache.commons.cli.UnrecognizedOptionException e
               (print-err [(.getMessage e)
                           (str "for help type '" f-key " -h'")]))
             (catch Exception e
               (print-err ["A UNEXPECTED ERROR OCCURED"
                           "Send error.edn file to the developer"])
               (spit "error.edn"
                     (pr-str {:args args-string
                              :f f
                              :e e})))))
          (pre)
          (recur (read-line)))))))
