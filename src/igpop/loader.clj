(ns igpop.loader
  (:require
   [clj-yaml.core]
   [clojure.java.io :as io]
   [clojure.data.csv :as csv]
   [clojure.string :as str]))

(defn read-yaml [pth]
  (clj-yaml.core/parse-string
   (slurp pth)))

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

(defn build-snapshot [base diff definitions]
  (assoc (merge base diff)
         :elements
         (merge-elements (set-elements-defaults (:elements base) {:collection false})
                         (:elements diff)
                         definitions)))

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



(defn build-profiles [ctx mode]
  (->> ctx
       :source
       (reduce
        (fn [acc [rt profiles]]
          (reduce (fn [acc [id profile]]
                    (assoc-in acc [rt id]
                              (cond
                                (= mode "profiles") (enrich ctx [rt] profile)
                                (= mode "resources") (get-in ctx (into [:base :profiles] [rt]))
                                (= mode "diff-profiles") (build-diff profile (:defaults ctx))
                                (= mode "snapshots") (build-snapshot (get-in ctx [:base :profiles rt])
                                                                     (get-in  ctx [:diff-profiles rt id])
                                                                     [:definitions ctx]))))
                  acc profiles)
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
    (-> (merge ctx user-data {:manifest manifest})
        (build-profiles "profiles")
        (build-profiles "resources")
        (build-profiles "diff-profiles")
        (build-profiles "snapshots"))))

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

(defn load-defaults []
  (let [defaults (safe-file "src/igpop/defaults.yaml")]
    (read-yaml defaults)))

(defn load-project [home]
  (let [manifest-file (io/file home "ig.yaml")]
    (when-not (.exists manifest-file)
      (throw (Exception. (str "Manifest " (.getPath manifest-file) " does not exists"))))

    (let [manifest (read-yaml (.getPath manifest-file))
          fhir (when-let [fv (:fhir manifest)] (load-fhir home fv))
          definitions (when-let [fv (:fhir manifest)] (load-definitions home fv))
          defaults (load-defaults)
          manifest' (assoc manifest :base fhir :home home :definitions definitions :defaults defaults)]
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


(comment

  )
