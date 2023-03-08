(ns core
  "https://veson.dev/blog -- statically generated"
  (:require [stasis.core :as stasis])
  (:require [clojure.java.shell])
  )

(stasis/slurp-directory "blog" #".*\.org$")

(comment
  (clojure.java.shell/sh "ls asdf")
  (clojure.java.shell/sh "nix-shell" "--run" "cat blog/test-entry.org | ./build.el")
  (clojure.java.shell/sh "nix-shell" "--run" "cat blog/test-entry.org | ./build.el | tidy --indent auto --show-body-only yes --quiet yes")
  )

(defn exec
  "Invoke me with clojure -X core/exec"
  [opts]
  (println "exec with" opts))

(defn -main
  "Invoke me with clojure -M -m core"
  [& args]
  (println "-main with" args))
