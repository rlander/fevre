(ns fevre.example
  (:use ring.adapter.jetty)
  (:use ring.middleware.reload)
  (:use ring.middleware.stacktrace)
  (:use ring.util.response)
  (:use fevre.core))

(defn hello [req]
  {:body "Hello World!"
   :headers {}
   :status 200})

(defn anything [req]
  (response (str "URI: " (:uri req))))

(def my-routes {#"/hello/$" hello
                #"./+" anything})
(def app
  (-> (router my-routes)
      (wrap-reload '(fevre.example))
      (wrap-stacktrace)))

(defn boot []
  (run-jetty #'app {:port 8080}))
