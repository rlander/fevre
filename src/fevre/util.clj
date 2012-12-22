(ns fevre.util
  (:require [clojure.string :as string]))

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

(defmulti resolve-ns class)
(defmethod resolve-ns clojure.lang.Var [v] v)
(defmethod resolve-ns clojure.lang.Fn [f] f)
(defmethod resolve-ns String [s]
  (let [[nspace func] (string/split s #"\/")]
  (try
    (require (symbol nspace))
    (ns-resolve (symbol nspace) (symbol func))
    (catch Exception e 
      (str "Unable to resolve " s ". Targets should be functions, vars or strings in the format [namespace/function]")))))
