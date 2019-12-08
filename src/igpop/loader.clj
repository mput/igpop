(ns igpop.loader
  (:require
   [clj-yaml.core]
   [clojure.java.io :as io]
   [clojure.data.csv :as csv]
   [clojure.string :as str]))

(defn read-yaml [pth]
  (clj-yaml.core/parse-string
   (slurp pth)))

(defn get-inlined-valuesets [{profiles :profiles valuesets :valuesets :as ctx}]
  (assoc ctx :valuesets (merge valuesets (:valuesets (reduce (fn [acc [rt prls]]
                                                                      (reduce (fn [acc [id {elements :elements :as pr}]]
                                                                                (reduce (fn [acc [eln el]]
                                                                                          (if (get-in el [:valueset :concepts])
                                                                                            (let [vs (:valueset el)
                                                                                                  vs-cnt (select-keys vs (for [[k v] vs :when (not (= k :id))]
                                                                                                                           k))]
                                                                                              (assoc-in acc [:valuesets (keyword (:id vs))] vs-cnt))
                                                                                            acc)) acc elements)) acc prls)) {} profiles)))))

(defn enrich [ctx pth obj]
  (let [base (-> (get-in ctx (into [:base :profiles] pth)) (dissoc :elements))]
    (if-let [els (:elements obj)]
      (let [els' (reduce (fn [acc [k v]]

                           (let [next-pth (into pth [:elements k])]
                             (if (get-in ctx (into [:base :profiles] next-pth))
                               (assoc acc k (enrich ctx next-pth v))
                               (if-let [tp (:type base)]
                                 (assoc acc k (enrich ctx [(keyword tp) :elements k] v))
                                 acc))))
                         els els)]
        (assoc (merge base obj) :elements els'))
      (merge base obj))))

(defn capitalized? [s]
  (when (string? s)
    (Character/isUpperCase (first s))))

(defn parse-name
  ([dir file-name]
   (let [parts (str/split file-name #"\.")]
     (cond
       (and (= 2 (count parts))
            (capitalized? dir)
            (= "yaml" (second parts)))

       {:to [:source (keyword dir) (keyword (first parts))]
        :format :yaml}

       (and
        (= 3 (count parts))
        (= "vs" (first parts))
        (= "yaml" (nth parts 2)))

       {:to [:valuesets (keyword (second parts))]
        :format :yaml}

       (and
        (= 3 (count parts))
        (= "vs" (first parts))
        (= "csv" (nth parts 2)))

       {:to [:valuesets (keyword (second parts)) :concepts]
        :format :csv}

       :else nil)))

  ([file-name]
   (let [parts (str/split file-name #"\.")]
     (cond
       (and (= 2 (count parts))
            (capitalized? (first parts))
            (= "yaml" (second parts)))

       {:to [:source (keyword (first parts)) :basic]
        :format :yaml}


       (and (= 3 (count parts))
            (= "vs" (first parts))
            (= "yaml" (nth parts 2)))

       {:to [:valuesets (keyword (second parts))]
        :format :yaml}

       (and (= 3 (count parts))
            (= "vs" (first parts))
            (= "csv" (nth parts 2)))

       {:to [:valuesets (keyword (second parts)) :concepts]
        :format :csv}


       :else
       nil))))

(defmulti read-file (fn [fmt _] fmt))
(defmethod read-file :yaml
  [_ pth]
  (read-yaml pth))

(defmethod read-file :csv
  [_ pth]
  (let [[headers & rows] (csv/read-csv (io/reader pth))
        ks (->> headers
                (mapv (fn [k] (keyword (str/trim k)))))]
    (->> rows
         (mapv (fn [rows]
                 (zipmap ks (mapv str/trim rows)))))))

(defn read-md-meta [content]
  (if (str/starts-with? content "---")
    (let [lines (str/split-lines content)]
      (loop [[l & ls] (rest lines)
             meta-lines []]
        (cond
          (nil? l) [{:error "Expected second --- to close metadata"} content]
          (str/starts-with? l "---")
          [(clj-yaml.core/parse-string (str/join "\n" meta-lines)) (str/join "\n" ls)]
          :else
          (recur ls (conj meta-lines l)))))
    [{} content]))

(defmethod read-file :md
  [_ pth]
  (let [content (slurp pth)
        [meta content] (read-md-meta content)]
    (assoc meta :source content)))

(defn merge-in [m pth v]
  (update-in m pth (fn [x] (if x (merge x v) v))))


;; (defn merge-elms [base-elms extending-elms]
;;   (reduce (fn [acc [k v]]
;;             (into acc {k (merge v (k extending-elms))}))
;;           {}
;;           base-elms))

(defn complex-type? [type] (capitalized? (name type)))
(defn get-ref-type [ref] (keyword (last (str/split ref #"/"))))

#_(defn type->elements [ctx type]
  (let [type-value (get-in ctx [:definitions :complex (keyword type)])]
    (->> (filter
          (fn [[key]] (not (or
                            (str/starts-with? (name key) "_")
                            (= key :Extension))))
          (:properties type-value))
         (map (fn [[subtype props]]
                [subtype (into (dissoc props :items :$ref)
                               (if-let [ref (get-in props [:items :$ref])]
                                 {:collection true :ref ref}
                                 (if-let [ref (get props :$ref)]
                                   {:ref ref})
                                 ))]))
         (map (fn [[subtype props]]
                [subtype (into (dissoc props :ref)
                               (if-let [ref-path (:ref props)]
                                 (let [ref-type (get-ref-type ref-path)]
                                   (if (complex-type? ref-type)
                                     {:elements (type->elements ctx ref-type)}
                                     {:type ref-type})))
                               )]))
         (into {}))))

(defn type->elements [ctx type]
  (let [type-type (if (complex-type? type) :complex :primitive)
        type-value (get-in ctx [:definitions type-type (keyword type)])]
    (if (= type-type :complex)
      (->> (filter
            (fn [[key]] (not (or
                              (str/starts-with? (name key) "_")
                              (= key :extension))))
            (:properties type-value))
           (map (fn [[subtype props]]
                  [subtype (into (dissoc props :items :$ref)
                                 (let [items (:items props)
                                       ref (:$ref (or items props))]
                                   (merge {:collection (boolean items)}
                                          (if-let [ref-type (and ref (get-ref-type ref))]
                                            (if (not (= ref-type :Reference))
                                              (type->elements ctx (get-ref-type ref))
                                              {:type {:complex false :name :Reference}})
                                            ))))]))
           ((fn [elements]
              {:type {:complex true :name type} :elements (into {} elements)}))
           )
      {:type {:complex false :name type}})))

(defn embed-complex-types [ctx elms]
  (reduce (fn [acc [name value]]
            (assoc acc name
                   (if-let [nested-elms (:elements value)]
                     (assoc value :elements (embed-complex-types ctx nested-elms) )
                     (if-let [type (:type value)]
                       (merge value (type->elements ctx type))
                       value))))
          {}
          elms))

(defn full-enrich [ctx pth extending-profile]
  (let [base-profile (-> (get-in ctx (into [:base :profiles] pth)))
        merged-base (merge (dissoc base-profile :elements) (dissoc extending-profile :elements))
        #_merged-elms #_(merge-elms (:elements base-profile) (:elements extending-profile))]
    (into merged-base {:elements (embed-complex-types ctx (:elements base-profile))})))

(defn build-profiles [ctx mode]
  (->> ctx
       :source
       (reduce
        (fn [acc [rt profiles]]
          (reduce (fn [acc [id profile]]
                    (assoc-in acc [rt id]
                              (cond
                                (= mode "profiles") (enrich ctx [rt] profile)
                                (= mode "full-profiles") (full-enrich ctx [rt] profile)
                                (= mode "resources") (-> (get-in ctx (into [:base :profiles] [rt])))
                                (= mode "diff-profiles") profile)
                               )) acc profiles)
          ) {})
       (assoc ctx (keyword mode))
       (get-inlined-valuesets)))


(defn load-defs [ctx pth]
  (let [manifest (read-yaml (str pth "/ig.yaml"))
        files (.listFiles (io/file (str pth "/src")))
        user-data (->> files
                       (sort-by #(count (.getName %)))
                       (reduce
                        (fn [acc f]
                          (let [nm (.getName f)]
                            (if (.isDirectory f)
                              (if (= nm "docs")
                                (reduce (fn [acc f]
                                          (let [parts (str/split (.getName f) #"\.")
                                                id (keyword (first parts))
                                                file-path (.getPath f)]
                                            (cond
                                              (= "md" (last parts))
                                              (assoc-in acc [:docs :pages id] (read-file :md file-path))
                                              (= "yaml" (last parts))
                                              (let [res (read-file :yaml file-path)]
                                                (update-in acc [:docs id] (fn [x] (if x (merge x res) res))))
                                              :else
                                              acc)))
                                        acc (.listFiles f))
                                (let [rt (keyword nm)]
                                  (reduce (fn [acc f]
                                            (if-let [insert (parse-name nm (.getName f))]
                                              (let [source (read-file (:format insert) (.getPath f))]
                                                (merge-in acc (:to insert) source))
                                              acc))
                                          acc (.listFiles f))))
                              (if-let [insert (parse-name nm)]
                                (let [source (read-file (:format insert) (.getPath f))]
                                  ;; (println "..." insert)
                                  (merge-in acc (:to insert) source))
                                (do (println "TODO:" nm)
                                    acc))))) {}))]
    ((build-profiles (build-profiles (merge ctx user-data) "profiles") "resources") "diff-profiles")
    #_(build-profiles (merge ctx user-data) "full-profiles"))
  )

(defn safe-file [& pth]
  (let [file (apply io/file pth)]
    (when (.exists file) file)))

(defn load-fhir [home fhir-version]
  (if-let [fhir-dir (safe-file home "node_modules" (str "igpop-fhir-" fhir-version) "src")]
    (->> (file-seq fhir-dir)
         (reduce (fn [acc f]
                   (let [nm (.getName f)]
                     (cond
                       (str/starts-with? nm "vs.")
                       (let [rt (str/replace nm #"\.yaml$" "")]
                         (assoc-in acc [:valuesets (keyword rt)]
                                   (read-yaml (.getPath f))))

                       (and (str/ends-with? nm ".yaml"))
                       (let [rt (str/replace nm #"\.yaml$" "")]
                         (assoc-in acc [:profiles (keyword rt)] (read-yaml (.getPath f))))
                       ))) {}))
    (println "Could not find " (.getPath (io/file home "node_modules" (str "igpop-fhir-" fhir-version))))))

(defn load-definitions [home fhir-version]
  (if-let [fhir-types (safe-file home "node_modules" (str "igpop-fhir-" fhir-version) "fhir-types-definition.yaml")]
    (read-yaml fhir-types)
    (println "Could not find " (.getPath (io/file home "node_modules" (str "igpop-fhir-" fhir-version "fhir-types-definition.yaml"))))))

(defn load-project [home]
  (let [manifest-file (io/file home "ig.yaml")]
    (when-not (.exists manifest-file)
      (throw (Exception. (str "Manifest " (.getPath manifest-file) " does not exists"))))

    (let [manifest (read-yaml (.getPath manifest-file))
          fhir (when-let [fv (:fhir manifest)] (load-fhir home fv))
          definitions (when-let [fv (:fhir manifest)] (load-definitions home fv))
          manifest' (assoc manifest :base fhir :home home :definitions definitions)]
      (merge
       manifest'
       (load-defs manifest' home)))))

(defn reload [ctx]
  (swap! ctx
         (fn [{home :home :as ctx}]
           (merge
            (dissoc ctx :profiles :sources :valuesets)
            (read-yaml (io/file home "ig.yaml"))
            (load-defs ctx home)))))



