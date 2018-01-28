(ns bucks.cli
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]
            [commandline.core :as cli-a]
            [slingshot.slingshot :refer [try+]]))


;;;; COMMANDS

(def source-param ["s" "source" "The Name of the Source" :string "S" true])
(def name-param ["n" "name" "Unique Name Identifier" :string "N" true])
(def date-param ["d" "date" "The Date of the Change. Can be yyyy-mm-dd" :time "D" true])
(def units-param ["u" "units" "A positive number representing units" :double "S" true])
(def age-param ["ag" "age" "Age" :integer "AG" true])
(def amount-param ["a" "amount" "Transaction Amount" :double "A" true])
(def value-param ["v" "value" "Asset Value" :double "V" true])
(def dob-param ["dob" "date-of-birth" "Date of Birth" :time "DOB" true])
(def note-param ["n" "note" "Note" :string "N" true])
(def year-param ["y" "year" "Year" :integer "Y" true])
(def precentage-param ["p" "percentage" "Percentage" :double "P" true])

(def commands
  {:add-salary ["salary" "Add a salary change" [source-param date-param value-param]]})


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
  (println)
  (println "Are these values correct? (y/n)")
  (let [v (read-line)]
    (if (contains? #{"y" "Y" "yes"} v)
      (do (f o) (println "Done"))
      (println "cancel"))))


(def hour-offset 2) ;; FAKE OUT RSA TIME BECAUSE I SUCK AT LOCALIZATION


(defn prep-date [k m]
  (->> (get m k)
       (#(.getMillis (.plusHours % hour-offset)))
       (assoc m :timestamp)))


(defn prep-dates [m]
  (cond->> m
    (contains? m :date-of-birth) (prep-date :date-of-birth)
    (contains? m :date) (prep-date :date)))


(defn prep-command [dispatch-command [k opt]]
  (concat
   (take 2 opt)
   [(fn [o] (->> (assoc o :type k) prep-dates (confirm-command dispatch-command)))]
   (drop 2 opt)))


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
         (pprint/print-table [:f :description]))))


;;;; ACTIONS

(def base-actions [(quit-opt "q") (quit-opt "e") (quit-opt "quit") (quit-opt "exit")])


(defn action-map [dispatch-command]
  (let [actions
        (->> commands
             (map (partial prep-command dispatch-command))
             (into base-actions)
             (map prep-help)
             (map (juxt first identity))
             (into {}))
        help-fn (print-help-fn actions)]
    (assoc actions "help" ["help" "Help" help-fn []])))


;;;;CLI

(defn pre [] (print "bucks> ") (flush))

(defn print-err [err]
  (cond
    (string? err) (println err)
    (coll? err)   (doseq [e err] (println e))
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
        (println "*****")
        (println name)
        (println detail)
        (print "Options")
        (->> options-spec
             (map (fn [[a b c d e]]
                    {:short (if a (str "-" a) "")
                     :long (if b (str "--" b) "")
                     :description c
                     :type d
                     :required (boolean e)}))
             (pprint/print-table [:short :long :description :type :required])))
      (f options))))


;;todo
;; parsing functions should be done better

(defn startup [dispatch-command]
  (let [dispatch (fn [command] (fn [o] (dispatch-command (command o))))
        fn-map   (action-map dispatch-command)]
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
                           "Send error.edn file to the developer"
                           (.getMessage e)])
               (spit "error.edn"
                     (pr-str {:args args-string
                              :f f
                              :e e})))))
          (pre)
          (recur (read-line)))))))
