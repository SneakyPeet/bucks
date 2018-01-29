(ns bucks.cli
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]
            [commandline.core :as cli-a]
            [slingshot.slingshot :refer [try+]])
  (:import java.util.Date
           jline.console.ConsoleReader))


(def console (ConsoleReader.))


(.setPrompt console "bucks> ")


(defn read-key [t]
  (println t)
  (.readCharacter console (char-array "ynYN")))


;;;; COMMANDS

(def source-param ["s" "source" "The Name of the Source" :string "S" true])
(def name-param ["n" "name" "Unique Name" :string "N" true])
(def asset-name-param ["n" "name" "Unique Asset Name" :string "N" true])
(def date-param ["d" "date" "The Date of the Change. Can be yyyy-mm-dd" :time "D" true])
(def units-param ["u" "units" "A positive number representing units" :double "U" true])
(def nr-units-param ["u" "units" "A positive number representing units" :double "U" false])
(def age-param ["ag" "age" "Age" :integer "AG" true])
(def amount-param ["a" "amount" "Transaction Amount" :double "A" true])
(def deposit-amount-param ["a" "amount" "Deposit Amount" :double "A" true])
(def withdraw-amount-param ["a" "amount" "Withdraw Amount" :double "A" true])
(def value-param ["v" "value" "Asset Value" :double "V" true])
(def dob-param ["dob" "date-of-birth" "Date of Birth" :time "DOB" true])
(def note-param ["nt" "note" "Note" :string "N" false])
(def year-param ["y" "year" "Year" :integer "Y" true])
(def percentage-param ["p" "percentage" "Percentage" :double "P" true])
(def asset-types #{"TFSA" "RA" "Crypto" "Savings" "Shares" "UnitTrust"})
(def asset-type-param ["at" "asset-type" (str "Asset type: " asset-types) :string "AT" true])
(def exclude-from-net-param
  ["ex" "exclude-from-net" "Should Asset Be Excluded From Net Calculations" :boolean "EN" true])

(def transaction-params [asset-name-param date-param amount-param nr-units-param note-param value-param])

(def commands
  {:add-salary ["salary" "Salaries: Add a Salary Change"
                [source-param date-param value-param]]
   :add-asset ["add-asset" "Assets: Add. Requires the initial amount"
               [asset-name-param date-param deposit-amount-param nr-units-param
                note-param asset-type-param exclude-from-net-param]]
   :close-asset ["close-asset" "Assets: Close. Requires the close value"
                 [asset-name-param date-param withdraw-amount-param note-param]]
   :make-deposit ["deposit" "Assets: Depost Into Asset" transaction-params]
   :make-withdrawal ["withdraw" "Assets: Withdraw amount from asset" transaction-params]
   :set-asset-value ["value" "Assets: Set value" [asset-name-param date-param value-param]]
   :set-date-of-birth ["dob" "WI: Set Bate of Birth for WI Calculations" [dob-param]]
   :add-wealth-index-goal ["add-wi-goal" "WI: Add WI Goal" [name-param age-param units-param]]
   :add-yearly-goal ["add-year-goal" "Goals: Add Year Growth Goal"
                     [name-param year-param percentage-param]]})


(def all-param-keys (->> commands
                         vals
                         (map last)
                         (reduce into)
                         (map second)
                         (map keyword)
                         set
                         doall))


(defn confirm-command [f o]
  (pprint/print-table [(select-keys o all-param-keys)])
  (.println console)
  (let [v (read-key "Are these values correct? (y/n) ")]
    (if (contains? #{"y" "Y" "yes"} v)
      (do (f o) (.println console "Done"))
      (.println console "cancel"))))


(def hour-offset 2) ;; FAKE OUT RSA TIME BECAUSE I SUCK AT LOCALIZATION


(defn prep-date [k rk m]
  (->> (get m k)
       (#(.getMillis (.plusHours % hour-offset)))
       (assoc m rk)))


(defn prep-dates [m]
  (cond->> m
    (contains? m :date-of-birth) (prep-date :date-of-birth :date-of-birth)
    (contains? m :date) (prep-date :date :timestamp)))


(defn prep-command [dispatch-command [k opt]]
  (concat
   (take 2 opt)
   [(fn [o] (->> (assoc o :type k) (confirm-command #(-> % prep-dates dispatch-command))))]
   (drop 2 opt)))


;;;; QUERIES


(defn prep-timestamp [k m]
  (update m k #(.toString (Date. %))))


(defn prep-timestamps [m]
  (cond->> m
    (contains? m :date-of-birth) (prep-timestamp :date-of-birth)
    (contains? m :timestamp) (prep-timestamp :timestamp)))


(defn prep-query [f query [k opt]]
  (concat
   (take 2 opt)
   [(fn [o] (f (query k o)))]
   (drop 2 opt)))


(def table-queries
  {:salaries ["salaries" "Salaries: View" []]
   :assets ["assets" "Assets: View" []]
   :asset-history ["asset-history" "Assets: History" [name-param]]
   :wi-goals ["wi-goals" "WI: View Goals" []]
   :goals ["goals" "Goals: View" []]})


(defn prep-table-query [query query-conf]
  (prep-query
   (fn [d]
     (let [d
           (->> d
                (sort-by :name)
                (sort-by :timestamp)
                (map prep-timestamps))]
       (if (empty? d)
         (.println console "No Data")
         (pprint/print-table d))))
   query
   query-conf))


;;;; HELP

(def help-opt ["h" nil "Help"])


(defn quit-opt [key] [key "Quit" (fn [_] (System/exit 0)) []])


(defn prep-help [[a b c d]] [a b c (conj d help-opt)])


(defn print-help-fn [actions]
  (fn [_]
    (println "Type '<function-name> -h' to see explanation. (example 'salary -h') ")
    (->> actions
         (map (fn [[_ [i d & r]]]
                {:f i :description d}))
         (sort-by :description)
         (pprint/print-table [:f :description]))))


;;;; ACTIONS

(def base-actions [(quit-opt "q") (quit-opt "e") (quit-opt "quit") (quit-opt "exit")])


(defn action-map [dispatch-command query]
  (let [actions
        (->> commands
             (map (partial prep-command dispatch-command))
             (into base-actions)
             (into (map (partial prep-table-query query) table-queries))
             (map prep-help)
             (map (juxt first identity))
             (into {}))
        help-fn (print-help-fn actions)]
    (assoc actions "help" ["help" "Help" help-fn []])))


;;;;CLI


(defn print-err [err]
  (cond
    (string? err) (.println console err)
    (coll? err)   (doseq [e err] (.println console e))
    :else         err))


(defn parse-input [s]
  (let [args (filter not-empty (string/split s #"\s+"))]
    {:f-key (first args)
     :args (rest args)}))


(defn commandline [[name detail f options-spec] args]
  (let [options-spec-h (if (contains? (set args) "-h") [help-opt] options-spec)
        options-map (mapv cli-a/option-map options-spec-h)
        [options _] (cli-a/parse-commandline options-map args)]
    (if (contains? options :h)
      (do
        (.println console "*****")
        (.println console name)
        (.println console detail)
        (.println console "Options")
        (->> options-spec
             (map (fn [[a b c d e f]]
                    {:short (if a (str "-" a) "")
                     :long (if b (str "--" b) "")
                     :description c
                     :type d
                     :required (boolean f)}))
             (sort-by :required)
             reverse
             (pprint/print-table [:required :short :long :description :type])))
      (f options))))


;;todo
;; parsing functions should be done better

(defn startup [dispatch-command query]
  (let [fn-map (action-map dispatch-command query)]
    (do
      (.println console "Welcome to Bucks Asset Tracker")
      (.println console "Type help for a list of commands")
      (loop [args-string (.readLine console)]
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
                           "Send error.edn file to the developer"
                           (.getMessage e)])
               (spit "error.edn"
                     (pr-str {:args args-string
                              :f f
                              :e e})))))
          (recur (.readLine console)))))))
