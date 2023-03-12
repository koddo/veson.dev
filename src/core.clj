(ns core
  "https://veson.dev/blog -- statically generated"
  (:require [stasis.core :as stasis])
  (:require [clojure.java.shell :as sh])
  )

(stasis/slurp-directory "blog" #".*\.org$")

(comment
  (sh/sh "ls")
  (sh/sh "nix-shell" "--run" "cat blog/test-entry.org | ./convert-org-from-stdin.el")
  (sh/sh "nix-shell" "--run" "cat blog/test-entry.org | ./convert-org-from-stdin.el | tidy --indent auto --show-body-only yes --quiet yes")

  )

(defn exec
  "Invoke me with clojure -X core/exec"
  [opts]
  (println "exec with" opts))

(defn -main
  "Invoke me with clojure -M -m core"
  [& args]
  (println "-main with" args))
