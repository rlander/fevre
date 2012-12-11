(ns fevre.core)

(defn add-slash [url]
  (if (.endsWith url "/") 
    url
    (str url "/")))

(defn find-first [pred coll]
  (first (filter pred coll))) 

(defn find-matching-url [url routes]
  (find-first (fn [[k v]] (re-find k url)) routes))

(defn router [routes]
  (fn [req]
    (let [[k v] (find-matching-url (:uri req) routes)]
      (when k
        (v req)))))
