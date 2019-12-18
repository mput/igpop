(ns igpop.builder
  (:require [clojure.string :as str]))

(defn capitalized? [s]
  (when (string? s)
    (Character/isUpperCase (first s))))

(defn complex-type? [type] (capitalized? (name type)))

(defn get-ref-type [ref] (keyword (last (str/split ref #"/"))))

(defn type->elements [type definitions]
  (let [type-type (if (complex-type? type) :complex :primitive)
        type-value (get-in definitions [type-type (keyword type)])]
    (if (= type-type :complex)
      (let [required (mapv keyword (:required type-value))]
        (->> (filter
              (fn [[key]] (not (or
                                (str/starts-with? (name key) "_")
                                (= key :extension))))
              (:properties type-value))
             (reduce
              (fn [acc [name value]]
                (assoc acc name
                       (into (dissoc value :items :$ref)
                             (let [collection? (contains? value :items)
                                   required? (contains? required name)
                                   ref (:$ref (or (:items value) value))
                                   type (if ref (get-ref-type ref) "code")]
                               (merge {:type type}
                                      {:collection collection?}
                                      (if required?
                                        (if collection?
                                          {:minItems 1}
                                          {:required true})
                                        {}))))))
              {})))
      {})))

(defn set-element-defaults [elm defaults]
  (reduce
   (fn [elm-acc [def-key def-value]]
     (let [final-value? (not (map? def-value))
           has-key? (contains? elm-acc def-key)]
       (cond
         (and final-value? has-key?) elm-acc
         (and final-value? (not has-key?)) (assoc elm-acc def-key def-value)
         (and (not final-value?) (not has-key?)) elm-acc
         (and (not final-value?) has-key?) (assoc elm-acc def-key (set-element-defaults (get elm-acc def-key) def-value))
         )
       ))
   elm defaults))


(defn set-elements-defaults [elms elms-defaults]
  (reduce
   (fn [acc [elm-key elm-val]]
     (if (= elm-key :extension)
       (assoc acc :extension (set-elements-defaults elm-val elms-defaults))
       (assoc acc elm-key
              (let [setted-elm-val (set-element-defaults elm-val elms-defaults)]
                (if-let [nested-elements (:elements elm-val)]
                  (assoc setted-elm-val :elements (set-elements-defaults nested-elements elms-defaults))
                  setted-elm-val)))
       ))
   {}
   elms))

(defn build-diff [profile defaults]
  (assoc (dissoc profile :examples)
         :elements
         (set-elements-defaults (:elements profile) (:elements defaults))))


(defn merge-elements [base-elms ig-elms difinitions]
  (reduce (fn [acc [key base-value]]
            (assoc acc key
                   (let [ig-value (get ig-elms key)
                         merged-values (merge base-value ig-value)
                         next-ig-elms (:elements ig-value)]
                     (if next-ig-elms
                       (let [next-base-elms (or (:elements base-value)
                                                (type->elements (:type base-value) difinitions))]
                         (assoc merged-values :elements
                                (merge-elements next-base-elms next-ig-elms difinitions)))
                       merged-values))))
          ig-elms
          base-elms))

(defn buil-snapshot [base diff definitions]
  (assoc (merge base diff)
         :elements
         (merge-elements (set-elements-defaults (:elements base) {:collection false})
                         (:elements diff)
                         definitions)))

(defn get-id [project-id rt rn]
  (let [common (str project-id "-" (str/lower-case (name rt)))]
    (if (= rn :basic)
      common
      (str common "-" (str/lower-case (name rn))))))

(defn build-profile [ctx ig-profile rt rn]
  (let [defaults (:defaults ctx)
        definitions (:definitions ctx)
        {profile-id :id url :url fhirVersion :fhir} (:manifest ctx)
        id (get-id profile-id rt rn)
        base-profile (get-in ctx [:base :profiles rt])
        base (dissoc (merge base-profile ig-profile) :elements :examples)
        differential (build-diff ig-profile defaults)
        snapshot (buil-snapshot base-profile differential definitions)
        ]
    (merge base
           {:id id
            :type rt
            :url url
            :fhirVersion fhirVersion
            :differential differential
            :snapshot snapshot})))