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
  ([req user]
    {:body (str "Hello " user "!")
     :headers {}
     :status 200}))

(defn sum [req f s]
  (response (str (+ (Integer. f) (Integer. s)))))

(def my-routes (app "/hello/{user}/" #'hello
                    "/hello/"        #'hello
                    ; only matches digit characters [0-9]
                    "/sum/{f:[0-9]+}/{s:[0-9]+}/" "fevre.example/sum"))

(def my-app
  (-> my-routes 
      (wrap-reload '(fevre.example))
      (wrap-stacktrace)))

(defn start
  ([] (start 8080))
  ([port] (run-jetty #'my-app {:port port :join? false})))

