(ns igpop.loader-test
  (:require [igpop.loader :as sut]
            [clj-yaml.core]
            [clojure.test :refer :all]
            [clojure.java.io :as io]
            [matcho.core :as matcho]))

(defn read-yaml [pth]
  (clj-yaml.core/parse-string
   (slurp pth)))

(deftest test-loader
  (testing "parse-name"

    (matcho/match
     (sut/parse-name "Patient.yaml")
     {:to [:source :Patient :basic]
      :format :yaml})

    ;; (matcho/match
    ;;  (sut/parse-name "pr.Patient.example.pt1.yaml")
    ;;  {:to [:source :Patient :basic :example :pt1]
    ;;   :format :yaml})

    (matcho/match
     (sut/parse-name "Patient" "lab.yaml")
     {:to [:source :Patient :lab]
      :format :yaml})

    (matcho/match
     (sut/parse-name "vs.dic1.yaml")
     {:to [:valuesets :dic1]
      :format :yaml})

    (matcho/match
     (sut/parse-name "vs.dic1.csv")
     {:to [:valuesets :dic1 :concepts]
      :format :csv})


    )

  (def project-path (.getPath (io/resource "test-project")))

  (def project (sut/load-project project-path))

  (io/file project-path "ig.yaml")
  (io/file project-path "node_modules" "igpop-fhir-4.0.0")

  (comment (matcho/match
           (:base project)
           nil))

  (matcho/match
   (:source project)
   {:Patient {:basic {:elements {}}}})

  (matcho/match
   (:Patient (:profiles project))
   {:lab-report {}
    :basic {}})

  (second (get-in project [:profiles :Patient :basic :elements]))

  (get-in project [:source :Patient :basic :description])
  (get-in project [:profiles :Patient :basic :elements :gender :valueset :id])

  (keys project)

  (keys project)

  (matcho/match
   (get-in project [:valuesets :dict1])
   {:concepts [{:code "male" :display "Male"}]})

  (is (not (nil? (get-in project [:docs :pages :welcome]))))
  (is (not (nil? (get-in project [:docs :menu]))))
  (get-in project [:docs :pages])
  )


(deftest set-defaults
  (matcho/match
   (sut/set-defaults
    {:elements {:identifier {}
                :gender {:valueset {:id "fhir:administrative-gender"}
                         :mustsupport false}}}
    {:mustsupport true
     :valueset {
                :strength "extensible"
                }})
   {:elements {:identifier {:mustsupport true}
               :gender {:valueset {:id "fhir:administrative-gender"
                                   :strength "extensible"}
                        :mustsupport false}}})

  )
