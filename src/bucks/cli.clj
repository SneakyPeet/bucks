(ns bucks.cli
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]
            [commandline.core :as cli-a]
            [slingshot.slingshot :refer [try+]]))

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

(def help-opt ["h" nil "Help"])

(defn quit-opt [key] [key "Quit" (fn [_] (System/exit 0)) []])

(defn fn-map [dispatch-command]
  (let [dispatch (fn [command] (fn [o] (dispatch-command (command o))))
        actions
        (->>
         [["salary"
           "Add a salary change"
           (dispatch nil)
           [["s" "source" "Salary Source" :string "S" true]
            ["d" "start-date" "Salary Start Date" :time "D"  true]
            ["sa" "salary-amount" "Salary Amount" :float "SA" true]]]
          (quit-opt "q") (quit-opt "e") (quit-opt "quit") (quit-opt "exit")]
         (map (fn [[a b c d]] [a [a b c (conj d help-opt)]]))
         (into {}))
        help-fn
        (fn [_]
          (println "Type '<function-name> -h' to see explanation. (example 'salary -h') ")
          (->> actions
               (map (fn [[_ [i d & r]]]
                      {:f i :description d}))
               (pprint/print-table [:f :description])))]
    (assoc
     actions
     "help" ["help" "Help" help-fn []])))


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
