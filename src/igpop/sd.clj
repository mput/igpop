(ns igpop.sd
  (:require [clojure.string :as str]))

(defn map-if-not-nil [& keyvals]
  (->> (apply hash-map keyvals)
       (filter (fn [[key val]] (not (nil? val))))
       (into {})))


(defn element-to-sd [val paths]
  (map-if-not-nil
   :path (str/join "." (map name paths))
   :min (let [{:keys [required minItems collection]} val
              default (if (some? collection) 0)]
          (cond
            required 1
            minItems minItems
            :else default))
   :max (let [{:keys [required disabled maxItems collection]} val
              default (if (some? collection)
                        (if collection "*" "1"))]
          (cond
            disabled 0
            maxItems maxItems
            :else default))
   :definitions (:description val)
   :mustSupport (:mustsupport val)
   :fixedCode (let [{const :constant} val]
                (when (string? const) const))
   :fixedCoding (let [{const :constant} val]
                  (when (map? const) const))
   :constraints (when-let [constraints (:constraints val)]
                  (map (fn [[key {:keys [expression description severity]}]]
                         {:key (name key)
                          :severity severity
                          :human description
                          :expression expression
                          })
                       constraints))
   ))

(defn format-elements
  [elements type-or-paths]
  (if (keyword? type-or-paths)
    (format-elements {type-or-paths elements} [])
    (reduce
     (fn [acc [elm-name elm-val]]
       (let [paths (conj type-or-paths elm-name)
             {child-elements :elements}  elm-val
             child-sd-elements (if child-elements (format-elements child-elements paths) [])]
         (concat acc [(element-to-sd elm-val paths)] child-sd-elements))
       )
     []
     elements)))

(defn to-sd [{type :type
              raw-snapshot :snapshot
              raw-differential :differential
              :as profile}]
  (let [snapshot (format-elements raw-snapshot type)
        differential (format-elements raw-differential type)]
    (assoc profile
           :snapshot snapshot
           :differential differential)))
