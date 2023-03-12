(ns core
  "https://veson.dev/blog -- statically generated"
  (:require [stasis.core :as stasis])
  (:require [clojure.java.shell :as sh])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as s])
  (:require [clojure.tools.reader.edn :as edn])
  )

(stasis/slurp-directory "blog" #".*\.org$")

(comment
  (sh/sh "ls")
  (sh/sh "nix-shell" "--run" "cat blog/test-entry.org | ./convert-org-from-stdin.el")
  (sh/sh "nix-shell" "--run" "cat blog/test-entry.org | ./convert-org-from-stdin.el | tidy --indent auto --show-body-only yes --quiet yes")

  )

(defn emacsclient-my-convert-org-to-html [file-path]
  (sh/sh "nix-shell" "--run"
         (str "emacsclient --socket-name='/run/user/1001/tmp.p5cJesu4Gb' --eval "
              "'(my-convert-org-to-html \"" file-path "\")'")))

(defn convert-org-str-to-html-str
  ([content] (convert-org-str-to-html-str content "from-str.org"))
  ([content temp-name]       ; temp-name is here mostly for debugging at the moment
   (let [temp-file (java.io.File/createTempFile ; example: ___10345426375354557964.from-str.org
                    "___"   ; prefix -- this argument must be at least 3 chars
                    (str "." temp-name)  ; suffix
                    (io/file ".my-intermediate-results"))       ; dir
         temp-abspath (.getAbsolutePath temp-file)
         the-html-result-path (s/replace temp-file #"\.org$" ".html")
         the-html-result-file (io/file the-html-result-path)]
     (spit temp-file content)
     (emacsclient-my-convert-org-to-html temp-abspath)
     (let [the-html-result-str (slurp the-html-result-file)]
       (.delete temp-file)
       (.delete the-html-result-file)
       the-html-result-str
       ))))




(defn exec
  "Invoke me with clojure -X core/exec"
  [opts]
  (println "exec with" opts))

(defn -main
  "Invoke me with clojure -M -m core"
  [& args]
  (println "-main with" args))
