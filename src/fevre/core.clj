(ns fevre.core
  (:require [fevre.util :as util]
            [clojure.string :as s]
            [clj-jregex.core :as jregex])
  (:use ring.middleware.reload
        ring.middleware.stacktrace
        ring.adapter.jetty
        clojure.tools.logging
        clj-logging-config.log4j)
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

(defn router []
  (fn [request]
      (let [routes @the-routes
            uri (URLDecoder/decode (:uri request))]
        (when-let [match (find-matching-url uri routes)]
          (if (coll? (:params match))
            (apply (:view-fun match) request (rest (jregex/re-find (:pattern match) uri)))
            ((:view-fun match) request))))))


(defn gen-route-map 
  "Creates a list of route-maps from a list of user routes and updates the atom the-routes."
   
   [user-routes]
   (map #(assoc (zipmap [:pattern :view-fun] %) :params nil) (partition 2 2 user-routes)))

(defn make-pattern 
  ([pat] (make-pattern pat [] []))
  ([pat pattern params]
   (let [param (last pat)
         regex "[^/]+"]
     (cond 
       (empty? pat) [(conj pattern "$") params]
       (= param "/") (recur (pop pat) (conj pattern (str "/")) params)
       :else (recur (pop pat) (conj pattern (str "(" param "" regex ")")) (conj params param))))))


(defn compile-route-map 
   "Takes a route-map:
   
      {:pattern \"/prefix/{one}/\" <- String
       :view-fun #'function        <- Function
       :param nil}                 <- Vector
   
   Creates a JRegex pattern and updates :pattern, extracts params and updates :params"
   
   [{:keys [pattern view-fun] :as route-map}]
   (let [[prefix & pat] (util/re-tokenize route-re pattern)
         rpat (vec (reverse pat))
         [valid-pattern params](make-pattern rpat)]
     {:pattern (->> (reduce conj [prefix] valid-pattern)
                    (s/join "")
                    jregex/re-pattern)
      :view-fun view-fun
      :params (map util/trim-first-last params)}))

(def app
  (-> (router)
      (wrap-reload '(fevre.example))
      (wrap-stacktrace)))

(defn start [user-routes]
  (do
    (set-logger!)
    (let [compiled-routes (map #(compile-route-map %) (gen-route-map user-routes))]
      (swap! the-routes concat compiled-routes)
      (run-jetty #'app  {:port 8080}))))
