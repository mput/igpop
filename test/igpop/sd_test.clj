(ns igpop.sd-test
  (:require [igpop.sd :as sut]
            [igpop.loader :refer [load-defaults]]
            [igpop.builder :refer [set-elements-defaults]]
            [clojure.test :refer :all]
            [matcho.core :as matcho]))

(def defaults (load-defaults))

(defn to-sd-with-defaults
  ([profile] (to-sd-with-defaults profile "Patient"))
  ([profile base-rt] (sut/elements-to-sd (set-elements-defaults profile (:elements defaults))
                      [base-rt])))

(deftest to-sd-formatter-test
  (testing "Cardinality singular"
    (def profile
      {:birthdate {:required true}
       :animal {:disabled true}})

    (matcho/match
     (to-sd-with-defaults profile "Patient")
     [
      {:path "Patient.birthdate"
       :mustSupport true
       :max 1}
      {:path "Patient.animal"
       :mustSupport true
       :max 0}
      ]))

  (testing "Cardinality collections"
    (def profile
      {:name {:minItems 1, :maxItems 10}
       :communication {:minItems 1 }})

    (matcho/match
     (to-sd-with-defaults profile "Patient")
     [
      {:path "Patient.name"
       :min 1
       :max 10}
      {:path "Patient.communication"
       :min 1
       :max "*"}
      ]))

  (testing "Fixed values"
    (def profile
      {:code {:constant "female"}
       :coding {:constant {:code "code-1"
                           :system "sys-1"}}})

    (matcho/match
     (to-sd-with-defaults profile "Patient")
     [
      {:path "Patient.code"
       :fixedCode "female"}
      {:path "Patient.coding"
       :fixedCoding {:code "code-1"
                     :system "sys-1"}}
      ]))

  (testing "FHIRPath rules"
    (def profile
      ;; FIXME check and fix this rule at example profiles.
      {:name {:constraints
              {:us-core-8 {:expression "family.exists() or given.exists()"
                           :description "Patient.name.given or Patient.name.family or both SHALL be present"
                           ;; TODO severity: error should be default value
                           :severity "error"
                           }}}})

    (matcho/match
     (to-sd-with-defaults profile)
     [
      {:constraints [{:severity "error"
                     :key "us-core-8"
                     :expression "family.exists() or given.exists()"
                     :human "Patient.name.given or Patient.name.family or both SHALL be present"
                     }]}
      ]))

  )
