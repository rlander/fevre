(ns fevre.example
  (:use fevre.core
        ring.util.response
        ring.middleware.reload
        ring.middleware.stacktrace
        ring.adapter.jetty
        clojure.tools.logging
        clj-logging-config.log4j))

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

(def my-routes (app "/hello/{name}/" #'hello
                    "/hello/"        #'hello
                    "/sum/{f}/{s}/"  #'sum))

(def myapp
  (-> my-routes 
      (wrap-reload '(fevre.example))
      (wrap-stacktrace)))

(defn start []
  (run-jetty #'myapp  {:port 8080 :join? false}))
