(ns igpop.sd
  (:require [clojure.string :as str]))


(defn get-cardinality-map
 [{required :required disabled :disabled min-items :minItems max-items :maxItems}]
  (let [min (cond
              disabled 0
              required 1
              :else (or min-items 0))
        max (cond
              disabled 0
              required 1
              :else (or max-items "*"))]
    {:min min :max max}))


(defn build-sd-elment [val paths]
  (let [path (str/join "." (map name paths))
        coordinality (get-cardinality-map val)]
    (merge
     {:path path}
     coordinality
     )))

(defn build-sd-elements
 [elements parent-paths]
 (reduce
  (fn [acc [elm-name elm-val]]
    (let [paths (conj parent-paths elm-name)
          child-elements (:elements elm-val)
          child-sd-elements (if child-elements (build-sd-elements child-elements paths) [])]
      (concat acc [(build-sd-elment elm-val paths)] child-sd-elements))
    )
  []
  elements)
 )


(defn to-sd [{type :type raw-snapshot :sprofile #_napshot raw-differential :differential :as profile}]
  (let [snapshot (build-sd-elements (:elements raw-snapshot) [type])]
    {:snapshot snapshot}))


(to-sd {:type "Patient"
        :snapshot {:elements
                   {:birthdate {:required true}
                    :animal {:disabled true}}}})

