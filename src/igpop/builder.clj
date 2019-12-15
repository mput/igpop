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
                                      (if collection? {:collection true} {})
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


(defn set-elements-defaults [elms elm-defaults]
  (reduce
   (fn [acc [elm-key elm-val]]
     (if (= elm-key :extension)
       (assoc acc :extension (set-elements-defaults elm-val elm-defaults))
       (assoc acc elm-key
              (let [setted-elm-val (set-element-defaults elm-val elm-defaults)]
                (if-let [nested-elements (:elements elm-val)]
                  (assoc setted-elm-val :elements (set-elements-defaults nested-elements elm-defaults))
                  setted-elm-val)))
       ))
   {}
   elms))

(defn build-diff [profile defaults]
  (assoc profile :elements (set-elements-defaults (:elements profile) defaults)))


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

(defn build-snapshot [base-profile ig-profile difinitions]
  (merge base-profile ig-profile {:elements (merge-elements (:elements base-profile) (:elements ig-profile) difinitions)}))

(defn build-profile [ctx ig-profile rt id]
  (let [defaults (:defaults ctx)
        difinitions (:difinitions ctx)
        {profile-id :id url :url fhirVersion :fhir} (:manifest ctx)
        base-profile (get-in ctx [:base :profiles])
        diff-elements (set-elements-defaults (:elements ig-profile) (:elements defaults))
        snapshot-elements (merge-elements (:elements base-profile) diff-elements difinitions)
        ]
    {:id profile-id
     :differential diff-elements
     :snapshot snapshot-elements}))
