(ns igpop.sd
  (:require [clojure.string :as str]))

(defn map-if-val-exits [& keyvals]
  (->> (apply hash-map keyvals)
       (filter (fn [[key val]] (not (nil? val))))
       (into {})))

(defn element-to-sd [val paths]
  (map-if-val-exits
   :path (str/join "." (map name paths))
   :min (let [{:keys [required disabled minItems]} val]
          (cond
            disabled 0
            required 1
            :else (or minItems 0)))
   :max (let [{:keys [required disabled maxItems]} val]
          (cond
            disabled 0
            required 1
            :else (or maxItems "*")))
   :definitions (:description val)
   :mustSupport (:mustsupport val)
   :fixedCode (let [{const :constant} val]
                (when (string? const) const))
   :fixedCoding (let [{const :constant} val]
                  (when (map? const) const))
   :constraints (when-let [{constraints :constraints} val]
                  (map (fn [[key {:keys [expression description severity]}]]
                         {:key (name key)
                          :severity severity
                          :human description
                          :expression expression
                          })
                       constraints))
   ))

(defn elements-to-sd
 [elements parent-paths]
 (reduce
  (fn [acc [elm-name elm-val]]
    (let [paths (conj parent-paths elm-name)
          {child-elements :elements}  elm-val
          child-sd-elements (if child-elements (elements-to-sd child-elements paths) [])]
      (concat acc [(element-to-sd elm-val paths)] child-sd-elements))
    )
  []
  elements))

(defn to-sd [{type :type
              raw-snapshot :snapshot
              raw-differential :differential
              :as profile}]
  (let [base (dissoc profile :snapshot :differential)
        snapshot (elements-to-sd raw-snapshot [type])
        differential (elements-to-sd raw-differential [type])
        ]
    (merge base
           {:snapshot snapshot
            :differential differential})))
