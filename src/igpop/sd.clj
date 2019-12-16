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

(defn map-if-val-exits [& keyvals]
  (->> (apply hash-map keyvals)
       (filter (fn [[key val]] (not (nil? val))))
       (into {})))

(defn build-sd-elment [val paths]
  (let [path (str/join "." (map name paths))
        coordinality (get-cardinality-map val)]
    (merge
     (map-if-val-exits
      :path path
      :definitions (:description val)
      :mustSupport (:mustsupport val))
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


(defn to-sd [{type :type
              raw-snapshot :snapshot
              raw-differential :differential
              :as profile}]
  (let [base (dissoc profile :snapshot :differential)
        snapshot (build-sd-elements raw-snapshot [type])
        differential (build-sd-elements raw-differential [type])
        ]
    (merge base
           {:snapshot snapshot
            :differential differential})))
