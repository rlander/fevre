(ns fevre.example
  (:use ring.util.response
        fevre.core))

(defn hello
  ([req]
    (response "Hello World!"))
  ([req name]
    {:body (str "Hello " name "!")
     :headers {}
     :status 200}))

(defn anything [req]
  (response (str "URI: " (:uri req) )))

(defn sum [req f s]
  (response (str (+ (Integer. f) (Integer. s)))))

(def my-routes ["/hello/{name}/" #'hello
                "/hello/"        #'hello
                "/sum/{f}/{s}/"  #'sum
                ".*"            #'anything])

(defn boot []
  (start my-routes))
