(ns igpop.site.core
  (:require
   [clojure.string :as str]
   [igpop.loader]
   [igpop.site.profiles]
   [igpop.site.valuesets]
   [igpop.site.docs]
   [igpop.site.views :as views]
   [igpop.sd :as sd]
   [org.httpkit.server]
   [ring.middleware.head]
   [ring.util.codec]
   [ring.util.response]
   [route-map.core]
   [igpop.site.utils :as u]
   [cheshire.core :refer :all]
   [clojure.java.io :as io]))

(defn welcome [ctx req]
  {:status 200
   :headers {"content-type" "text/html"}
   :body (views/layout ctx
          [:div#content
           [:h1 "Hello"]])})

(defn handle-static [{meth :request-method uri :uri :as req}]
  (when (and (#{:get :head} meth)
           (or (str/starts-with? (or uri "") "/static/")
               (str/starts-with? (or uri "") "/favicon.ico")))
    (let [opts {:root "public"
                :index-files? true
                :allow-symlinks? true}
          path (subs (ring.util.codec/url-decode (:uri req)) 8)]
      (-> (ring.util.response/resource-response path opts)
          (ring.middleware.head/head-response req)))))

(defn source [ctx req]
  {:status 200
   :body (clj-yaml.core/generate-string (dissoc ctx :fhir))})


(defn temp [ctx req]
  {:status 200
   :body (clj-yaml.core/generate-string (:Patient (:complete-profiles ctx )))})

(defn handle-sd [ctx {{rt :resource-type nm :profile} :route-params}]
  (let [sd (sd/to-sd ctx (keyword rt) (keyword nm))]
    {:status 200
     :body (generate-string sd {:pretty true})}))

(defn profile-dispatch
  [ctx {{profile-with-format :profile-with-format} :route-params :as req}]
  (let [formats (partition 2 [".schema.json" nil
                              ".json" handle-sd
                              "" #'igpop.site.profiles/profile])]
    (some (fn [[end-var handler]]
            (if (str/ends-with? profile-with-format end-var)
              (handler ctx (assoc-in req
                                     [:route-params :profile]
                                     (str/replace profile-with-format (re-pattern end-var) "")))))
          formats)))

(def routes
  {:GET #'welcome
   "ig.yaml" {:GET #'source}
   "temp" {:GET #'temp}
   "docs" {:GET #'igpop.site.docs/dashboard
           [:doc-id] {:GET #'igpop.site.docs/doc-page}}
   "valuesets" {:GET #'igpop.site.valuesets/valuesets-dashboard
                [:valuset-id] {:GET #'igpop.site.valuesets/valueset}}
   "profiles" {:GET #'igpop.site.profiles/profiles-dashboard
               [:resource-type] {:GET #'igpop.site.profiles/profile
                                 [:profile-with-format] {:GET profile-dispatch}}}})

(dissoc (route-map.core/match [:get "/profiles/Patient/basic"] routes) :parents)
;; => {:match #'igpop.site.profiles/profile, :w 32, :params {:resource-type "Patient", :handler #'igpop.site.profiles/profile, :profile "basic"}}


(defn *dispatch [ctx {uri :uri meth :request-method :as req}]
  (let [uri (str/replace uri #"\.html$" "")
        req (assoc req :uri uri)]
    (if-let [{handler :match params :params} (route-map.core/match [meth uri] #'routes)]
      (handler ctx (assoc req :route-params params))
      {:status 200 :body "Ok"})))

(defn dispatch [ctx {uri :uri meth :request-method :as req}]
  (or
   (handle-static req)
   (do
     (igpop.loader/reload ctx)
     (*dispatch @ctx req))))

(defn mk-handler [home]
  (let [ctx (atom (igpop.loader/load-project home))]
    (fn [req]
      (dispatch ctx req))))

(defn start [home port]
  (let [h (mk-handler home)]
    (println "Run server on http://localhost:" port)
    (org.httpkit.server/run-server h {:port port})))

(defn dump-page [ctx home pth & [idx]]
  (let [href (apply u/href {} pth)
        {body :body} (*dispatch ctx {:request-method :get :uri href})
        [pth opts] (if (map? (last pth)) [(butlast pth) (last pth)] [pth {}])
        pth (if-let [root (:root opts)]
              (into [root] pth)
              pth)
        output (apply io/file (into [home "build"]
                                    (if idx
                                      (into pth ["index.html"])
                                      (if-let [fmt (:format opts)]
                                        (into  (vec (butlast pth))
                                               [(str (last pth) "." fmt)])
                                        pth))))]
    (println "Build.." href " => " (.getPath output))
    (.mkdir (apply io/file (into [home "build"] (butlast pth))))
    (spit (.getPath output) body)))

(defmacro get-static []
  (let [r (clojure.string/join " " (for [f (->> (str (System/getProperty "user.dir") "/resources" "/public")
                                                clojure.java.io/file
                                                file-seq
                                                (filter #(not (.isDirectory %))))]
                                     (.getName f)))]
    `~r))

(defn build [home base-url]
  (let [ctx (-> (igpop.loader/load-project home)
                (assoc :base-url base-url))]
    (.mkdir (io/file home "build"))
    (.mkdir (io/file home "build" "static"))
    (.mkdir (io/file home "build" "profiles"))
    (.mkdir (io/file home "build" "fhir"))
    (.mkdir (io/file home "build" "fhir" "profiles"))

    (dump-page ctx home [] :index)
    (dump-page ctx home ["profiles"] :index)

    (doseq [[rt prs] (:profiles ctx)]
      (doseq [[id pr] (if-not (some #(= % :basic) (keys prs))
                        (assoc prs :basic {})
                        prs)]
        (dump-page ctx home ["profiles" (name rt) (name id) {:format "html"}])
        (dump-page ctx home ["profiles" (name rt) (name id) {:format "json" :root "fhir"}])))

    (.mkdir (io/file home "build" "valuesets"))
    (dump-page ctx home ["valuesets"] :index)
    (doseq [[id _] (get-in ctx [:valuesets])]
      (dump-page ctx home ["valuesets" (name id) {:format "html"}]))

    (.mkdir (io/file home "build" "docs"))
    (dump-page ctx home ["docs"] :index)
    (doseq [[id _] (get-in ctx [:docs :pages])]
      (dump-page ctx home ["docs" (name id) {:format "html"}]))

    (doseq [f (str/split (get-static) #" ")]
      (when-not (or (= f "static-resources") (= f "static-resources\n"))
        (io/copy (io/input-stream (io/resource (str "public/" f))) (io/file home "build" "static" f))))
    ))

(comment

  (def hm (.getAbsolutePath (io/file  "example")))

  (def srv (start hm 8899))

  (build hm "http://localhost/igpop/example/build")
  (build hm "/igpop")

  (srv)

  (handler {:uri "/" :request-method :get})


  ;; try SD getn.
  (def home (io/file  "example"))
  (def proj (igpop.loader/load-project home))
  (def sd-exmpl (sd/to-sd proj :Patient :basic))

  (clojure.pprint/pprint (get-in proj [:snapshots :Patient :basic :elements]))
  (clojure.pprint/pprint (get-in sd-exmpl []))

  )
