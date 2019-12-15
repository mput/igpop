(ns igpop.sd-test
  (:require [igpop.sd :as sut]
            [clojure.test :refer :all]
            [matcho.core :as matcho]))

(deftest to-sd-formatter-test

  (testing "Cardinality singular"
    (def profile
      {:type "Patient"
       :snapshot {:elements
                  {:birthdate {:required true}
                   :animal {:disabled true}}}})

    (matcho/match
     (sut/to-sd profile)
     {:snapshot
      [
       {:path "Patient.birthdate"
        :max 1}
       {:path "Patient.animal"
        :max 0}
       ]}))

  (testing "Cardinality collections"
    (def profile
      {:type "Patient"
       :snapshot {:elements
                  {:name {:minItems 1, :maxItems 10}
                   :communication {:minItems 1 }}}})

    (matcho/match
     (sut/to-sd profile)
     {:snapshot
      [
       {:path "Patient.name"
        :min 1
        :max 10}
       {:path "Patient.communication"
        :min 1
        :max "*"}
       ]}))
  )
