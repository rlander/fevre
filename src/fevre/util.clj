(ns fevre.util
  (:use [clojure.string :as string :only [blank?]]))

(defn re-tokenize [re text]
  (let [matcher (re-matcher re text)]
    (defn inner [last-index result]
      (if (.find matcher)
        (let [start-index (.start matcher)
              end-index (.end matcher)
              match (.group matcher)
              insert (subs text last-index start-index)]
          (if (string/blank? insert)
            (recur end-index (conj result match))
            (recur end-index (conj result insert match))))
        (conj result (subs text last-index))))
    (inner 0 [])))

(defn trim-first-last [s]
  (subs s 1 (- (.length s) 1)))