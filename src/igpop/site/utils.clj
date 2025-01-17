(ns igpop.site.utils
  (:require [clojure.string :as str]))


(defn href [ctx & pth]
  (let [[pth opts] (if (map? (last pth))
                     [(butlast pth) (last pth) ]
                     [pth {}])
        fmt (when-let [fmt (:format opts)]
              (str "." fmt))
        res (if-let [bu (:base-url ctx)]
              (str (str/join "/" (into [bu] pth)) fmt)
              (str "/" (str/join "/" pth) fmt))]
    ;; (println "href:" res)
    res))

