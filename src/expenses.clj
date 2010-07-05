(comment
  Copyright 2009 Mark Triggs

  Licensed under the Apache License, Version 2.0 (the "License"); you may
  not use this file except in compliance with the License. You may obtain
  a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
    implied. See the License for the specific language governing
    permissions and limitations under the License.
)

(ns expenses
  (:import (java.util Calendar Date)
           (java.text SimpleDateFormat)
           (java.io File))
  (:use clojure.contrib.duck-streams
        clojure.contrib.str-utils)
  (:gen-class :name Expenses
              :main true))


(def *time-periods* {"weekly" 1
                     "fortnightly" 2
                     "monthly" 4
                     "yearly" 52})


(defn date-formatter []
  (SimpleDateFormat. "dd/MM/yyyy"))


(defn tokenise [s]
  "Split an input line into its parts."
  (let [s (. s trim)]
    (if (re-matches #"^\[.*" s)
      (map #(.trim %) (. s split "([\t ]{2,}|\t+)"))
      (concat [nil] (map #(.trim %) (. s (split "([\t ]{2,}|\t+)")))))))


(defn parse-applicability [s]
  "Parse a date range indicating when an entry is applicable."
  (let [date-parser (date-formatter)
        [_ start _ end]
        (first (re-seq #"\[([0-9]+/[0-9]+/[0-9]+)?(--)?([0-9]+/[0-9]+/[0-9]+)?\]"
                       s))]
    [(when start
       (. date-parser (parse start)))
     (when end
       (. date-parser (parse end)))]))


(defn parse-line [str]
  "Parse a line from our ~/.expenses file."
  (let [date-parser (date-formatter)
        [applicability date amount desc] (tokenise str)
        [from to] (if applicability
                    (parse-applicability applicability)
                    [nil nil])]
    {:from from
     :to to
     :date (if ((set (keys *time-periods*)) date)
             date
             (. date-parser (parse date)))
     :amount (. Float (valueOf amount))
     :description desc}))


(defn record [entry type summary]
  "Append an expense of `type' to our summary."
  (update-in summary [type] conj entry))


(defn normalise [entry]
  "Break a recurring expenditure down to its per-week amount."
  (dissoc (update-in entry
                     [:amount]
                     #(/ %
                         (*time-periods* (:date entry))))
          :date))


(defn sort-expenses [expenses]
  "Sort a list of expenses by date."
  (sort-by :date expenses))


(defn parse-expenses [stream]
  "Parse the expenses file and return a summary."
  (let [result (reduce (fn [result line]
                         (if (or (re-matches #"^[ \t]*$" line)
                                 (re-matches #"^#.*$" line))
                           result
                           (let [entry (parse-line line)]
                             (if (instance? Date (:date entry))
                               (record entry :expenses result)
                               (record (normalise entry) :weekly-expenses result)))))
                       {:weekly-expenses []
                        :expenses []}
                       (line-seq stream))]
    (update-in result [:expenses] sort-expenses)))



(defn week-of [date]
  "Find the beginning of the week containing `date'."
  (let [cal (doto (. Calendar getInstance)
              (.setTime date))]
    (if (not= (. cal (get Calendar/DAY_OF_WEEK))
              (. cal getFirstDayOfWeek))
      (recur (. (doto cal
                  (. add Calendar/DAY_OF_WEEK -1))
                getTime))
      date)))


(defn week-range [start end]
  "Enumerate the weeks between two dates.
For example.  (week-range 01/01/01 31/12/01) should yield 52 elements."
  (let [cal (doto (. Calendar getInstance)
              (.setTime start))]
    (take-while #(<= (. % (compareTo end)) 0)
                (map #(. % getTime)
                     (iterate (fn [c]
                                (. c (add Calendar/WEEK_OF_YEAR 1))
                                (. c clone))
                              cal)))))


(defn line
  "Return an ugly ASCII line."
  ([] (line 100))
  ([n] (apply str (replicate n "="))))


(defn entry-amounts [entries]
  "Sum the amounts of a list of entries"
  (reduce + (map :amount entries)))


(defn format-amount [amount]
  "Pretty-print a dollar amount."
  (format (if (>= amount 0) "(%7.2f)" "%7.2f")
          (float (Math/abs amount))))


(defn show-week [week-summary]
  "Show a report a given week-summary."
  (println (str "\n\n" (line)))
  (println (str "Week starting: " (:start-date week-summary)))
  (println (str (line) "\n"))
  (println "  Recurring items:\n")

  (doseq [entry (sort-by :description (:weekly-entries week-summary))]
    (println (format "    %s\t\t%-30s\t\t%8s"
                     (. (date-formatter) (format (:start-date week-summary)))
                     (:description entry)
                     (format-amount (:amount entry)))))

  (println (format "\n    Subtotal: %59s"
                   (format-amount (entry-amounts (:weekly-entries week-summary)))))

  (println "")
  (println "  Line items:\n")

  (doseq [entry (:entries week-summary)]
    (println (format "    %s\t\t%-30s\t\t%8s"
                     (. (date-formatter) (format (:date entry)))
                     (:description entry)
                     (format-amount (:amount entry)))))

  (println (format "\n    Subtotal: %59s"
                   (format-amount (entry-amounts (:entries week-summary)))))
  (println "")

  (println (str "  " (line 25)))
  (println (format "   Total saved: %s"
                   (format-amount
                    (entry-amounts
                     (lazy-cat (:weekly-entries week-summary)
                               (:entries week-summary))))))
  (println (str "  " (line 25))))


(defn tally-week [week summary]
  "Produce a summary of a given `week'"
  {:start-date week
   :entries (filter #(= week (week-of (:date %))) (:expenses summary))
   :weekly-entries (filter #(and (or (not (:from %))
                                     (>= (. week (compareTo (week-of (:from %))))
                                         0))
                                 (or (not (:to %))
                                     (< (. week (compareTo (week-of (:to %))))
                                        0)))
                           (:weekly-expenses summary))})



(defn generate-report [summary]
  "Print a report based on `summary'"
  (when (not (empty?  (:expenses summary)))
    (let [start-week (week-of (:date (first (:expenses summary))))
          end-week (week-of (:date (last (:expenses summary))))
          weeks (map #(tally-week % summary) (week-range start-week end-week))
          savings (reduce (fn [total week-summary]
                            (+ total
                               (entry-amounts (:entries week-summary))
                               (entry-amounts (:weekly-entries week-summary))))
                          0
                          weeks)]
      (doseq [week-summary weeks]
        (show-week week-summary))

      (println)
      (println (line 25))
      (println (format " Total savings (%s to %s):\t\t\t%s"
                       (. (date-formatter) (format start-week))
                       (. (date-formatter) (format end-week))
                       (format-amount savings)))
      (println (format "\n Average saved per week:\t\t\t\t\t%s"
                       (format-amount (/ savings
                                         (count weeks)))))
      (println (line 25)))))



(defn -main [& args]
  (when (not= (count args) 1)
    (. System/err (println "Usage: java -jar expenses.jar <expenses file>"))
    (. System (exit 0)))
  (let [file (try (reader (first args))
                  (catch Exception e
                    (. System/err (println (str "Failed to open "
                                                (first args))))
                    (. System (exit 1))))]
    (generate-report (parse-expenses file))))
