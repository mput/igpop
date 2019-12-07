(ns igpop.structure-def-test
  (:require [igpop.structure-def :as sdef]
            [clojure.test :refer :all]
            [matcho.core :as matcho]
            [clojure.java.io :as io]
            [igpop.loader :as loader]
            [cheshire.core :refer :all]))

(deftest test-structure-gen

  (def project-path (.getPath (io/resource "test-project")))

  (def project (loader/load-project project-path))

  (comment
    (sdef/generate-structure project)

    (get-in project [:profiles :Patient :basic :elements :name :elements :given])

    (spit (io/file (str (System/getProperty "user.dir") "/test-structure-def.json")) (generate-string (sdef/generate-structure project) {:pretty true}))

    ))
