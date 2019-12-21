(ns igpop.sd-test
  (:require [igpop.sd :as sut]
            [igpop.loader :refer [build-diff]]
            [clojure.test :refer :all]
            [matcho.core :as matcho]))

(def defaults (load-defaults))

(defn prepare
  ([profile] (prepare profile :Patient))
  ([profile base-rt] (sut/format-elements (build-diff profile defaults) base-rt)))

(deftest to-sd-formatter-test
  (testing "Cardinality singular"
    (def profile
      {:elements {:birthdate {:required true}
                  :animal {:disabled true}}})

    (matcho/match
     (prepare profile)
     [{:path "Patient"}
      {:path "Patient.birthdate"
       :mustSupport true
       :min 1}
      {:path "Patient.animal"
       :mustSupport true
       :max 0}]))

  (testing "Cardinality collections"
    (def profile
      {:elements {:name {:minItems 1 :maxItems 10}
                  :communication {:disabled true}}})

    (matcho/match
     (prepare profile)
     [{}
      {:path "Patient.name"
       :min 1
       :max 10}
      {:path "Patient.communication"
       :max 0}]))

  (testing "Fixed values"
    (def profile
      {:elements {:code {:constant "female"}
                  :coding {:constant {:code "code-1"
                                      :system "sys-1"}}}})

    (matcho/match
     (prepare profile)
     [{}
      {:path "Patient.code"
       :fixedCode "female"}
      {:path "Patient.coding"
       :fixedCoding {:code "code-1"
                     :system "sys-1"}}]))

  (testing "FHIRPath rules"
    (def profile
      ;; FIXME check and fix this rule at example profiles.
      {:elements {:name {:constraints
                         {:us-core-8 {:expression "family.exists() or given.exists()"
                                      :description "Patient.name.given or Patient.name.family or both SHALL be present"
                                      ;; TODO severity: error should be default value
                                      :severity "error"
                                      }}}}})

    (matcho/match
     (prepare profile)
     [{}
      {:constraints [{:severity "error"
                     :key "us-core-8"
                     :expression "family.exists() or given.exists()"
                     :human "Patient.name.given or Patient.name.family or both SHALL be present"
                     }]}]))
  
  )
