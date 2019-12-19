(ns igpop.sd
  (:require [clojure.string :as str]))

(defn map-if-not-nil [& keyvals]
  (->> (apply hash-map keyvals)
       (filter (comp some? val))
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

(defn get-id [project-id rt rn]
  (let [common (str project-id "-" (str/lower-case (name rt)))]
    (if (= rn :basic)
      common
      (str common "-" (str/lower-case (name rn))))))

(defn to-sd [ctx rt rn]
  (let [definitions (:definitions ctx)
        {profile-id :id url :url fhirVersion :fhir} (:manifest ctx)]
    (map-if-not-nil
     :id (get-id profile-id rn rn)
     :type (name rt)
     :url url
     :fhirVersion fhirVersion
     :snapshot (if-let [snap (get-in ctx [:snapshots rt rn])]
                 (format-elements snap rt))
     :differential (if-let [diff (get-in ctx [:diff-profiles rt rn])]
                     (format-elements diff rt))
     )))
