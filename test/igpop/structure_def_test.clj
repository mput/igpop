(ns igpop.structure-def-test
  (:require [igpop.structure-def :as sdef]
            [clojure.test :refer :all]
            [matcho.core :as matcho]
            [clojure.java.io :as io]
            [igpop.loader :as loader]
            [cheshire.core :refer :all]))

(deftest test-structure-gen

  (def profile
    {:Patient
     {:elements
      {:name {:type "HumanName"
              :minItem 1
              :elements
              {:family {:type "string" :isCollection true :minItem 1 :maxItem 10}}}}}})

  (testing "collection cardinality"
    (matcho/match
     (sdef/to-sd profile)
     {:snapshot [{:path "Patient.name" :min 1}]}))
  

  )

