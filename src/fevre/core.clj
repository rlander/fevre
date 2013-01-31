(ns fevre.core
  (:require [fevre.util :as util]
            [clojure.string :as s]
            [clj-jregex.core :as jregex])
  (:import (java.net URLDecoder)))

(def the-routes (atom []))

(def route-re #"(\{[a-zA-Z][^\}]*\})")

(defn add-slash [url]
  (if (.endsWith url "/") 
    url
    (str url "/")))

(defn find-first [pred coll]
  (first (filter pred coll))) 

(defn find-matching-url [uri routes]
  (find-first (fn [route] 
                (jregex/re-find (:pattern route) uri)) routes))

(defn gen-route-map 
  "Takes a list with route pairs (pattern, view-fun) and generates a route-map data structure
   (HashMap) for each pair.
    in:
      <List>
    out:
      <List>:
        {:pattern <String>
         :view-fun <Var | Fn | String>
         :param nil}"
         
   [user-routes]
   (map #(assoc (zipmap [:route :view-fun] %) :params nil) (partition 2 2 user-routes)))

(defn split-name-regex [s]
  (let [s (util/trim-first-last s)
        default-regex #"[^/]+"]
    (if (re-find #":" s)
      (s/split s #":")
      [s default-regex])))

(defn make-pattern 
  ([pat] (make-pattern pat [] []))
  ([pat pattern params]
   (let [param (last pat)]
     (cond 
       (empty? pat) [(conj pattern "$") params]
       (= param "/") (recur (pop pat) (conj pattern (str "/")) params)
       :else (let [[pname regex] (split-name-regex param)]
               (recur (pop pat) (conj pattern (str "({" pname "}" regex ")")) (conj params pname)))))))

(defn compile-route-map 
   "Compiles the user-defined route and extracts the arguments:
   
    in:
      {:pattern <String>
       :view-fun <Var | Fn | String>
       :params nil}
    
    out:
      {:pattern <JRegex>
       :view-fun <Var>
       :params <Vector>}"
   
   [{:keys [route view-fun] :as route-map}]
   (let [[prefix & pat] (util/re-tokenize route-re route)
         rpat (vec (reverse pat))
         [valid-pattern params](make-pattern rpat)]
     {:pattern (->> (reduce conj [prefix] valid-pattern)
                    (s/join "")
                    jregex/re-pattern)
      :view-fun (util/resolve-ns view-fun)
      :params params
      :route route}))

;; 
;; Dispatch

(defn urls [user-routes]
  (let [compiled-routes (map #(compile-route-map %) (gen-route-map user-routes))]
      (swap! the-routes concat compiled-routes)))

(defn dispatcher []
  (fn [request]
      (let [routes @the-routes
            uri (URLDecoder/decode (:uri request))]
        (when-let [match (find-matching-url uri routes)]
          (if (coll? (:params match))
            (apply (:view-fun match) request (rest (jregex/re-find (:pattern match) uri)))
            ((:view-fun match) request))))))

(defmacro app [& forms]
  `(do 
     (urls (vector ~@forms))
     (dispatcher)))
