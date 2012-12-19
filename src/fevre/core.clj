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

(def route_re #"(\{[a-zA-Z][^\}]*\})")

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
  "Creates a list of hashmaps from a list of routes and updates the-routes. 
  Ex.:
   user=> (gen-route-map \"^/hello$\" #'hello \"^/hello/([^/]+)\" #'anything)
   ({:params nil, :view-fun #'user/hello, :pattern \"^/hello$\"} {:params nil, :view-fun #'user/anything, :pattern \"^/hello/([^/]+)\"})"
   
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

(defn compile-route [{:keys [pattern view-fun] :as route-map}]
  (let [[prefix & pat] (util/re-tokenize route_re pattern)
         rpat (vec (reverse pat))
         [compiled-pattern params](make-pattern rpat)]
    {:pattern (jregex/re-pattern (s/join "" (reduce conj [prefix] compiled-pattern)))
     :view-fun view-fun
     :params (map util/trim-first-last params)}))

(def app
  (-> (router)
      (wrap-reload '(fevre.example))
      (wrap-stacktrace)))

(defn start [user-routes]
  (do
    (set-logger!)
    (let [compiled-routes (map #(compile-route %) (gen-route-map user-routes))]
      (swap! the-routes concat compiled-routes)
      (run-jetty #'app  {:port 8080}))))
