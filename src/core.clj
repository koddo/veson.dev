(ns core
  "https://veson.dev/blog -- statically generated"
  (:require [stasis.core :as stasis])
  )

(stasis/slurp-directory "blog" #".*\.org$")

(defn exec
  "Invoke me with clojure -X core/exec"
  [opts]
  (println "exec with" opts))

(defn -main
  "Invoke me with clojure -M -m core"
  [& args]
  (println "-main with" args))
