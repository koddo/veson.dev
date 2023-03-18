(ns core
  "https://veson.dev/blog -- statically generated"
  (:require [stasis.core :as stasis])
  (:require [clojure.java.shell :as sh])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as s])
  (:require [clojure.tools.reader.edn :as edn])
  (:require [lambdaisland.regal :as regal])
  )



(comment
  (sh/sh "ls")
  (sh/sh "nix-shell" "--run" "cat blog/test-entry.org | ./convert-org-from-stdin.el")
  (sh/sh "nix-shell" "--run" "cat blog/test-entry.org | ./convert-org-from-stdin.el | tidy --indent auto --show-body-only yes --quiet yes")


  (defn pages-unprocessed [stasis-struct]
    (for [[filename file-contents-string]
          stasis-struct]
      (let [[_ front-matter-edn body-org] (re-matches front-matter-and-body-regex file-contents-string)]
        {:filename filename
         :front-matter-edn front-matter-edn
         :body-org body-org})))

  (defn process-pages [pages-unprocessed]
    (for [{:keys [filename front-matter-edn body-org] :as p} pages-unprocessed]
      (assoc p
             :filename-export (s/replace filename #"\.org$" ".html")
             :front-matter (edn/read-string front-matter-edn)
             :body-html (convert-org-str-to-html-str body-org))))
  
  
  (let [all-pages (pages-unprocessed (stasis/slurp-directory "blog" #".*\.org$"))
        broken-pages (for [p all-pages
                           :when (nil? (:front-matter-edn p))]
                       p)]
    (if-not (empty? broken-pages)
      (str "Can't recognize front matter in these files: " (seq broken-pages))
      (process-pages all-pages)
      ))
  
    )  

;; example
;; #+name: front-matter   
;; #+begin_src clojure 
;; {whatever}
;; #+end_src
;; * hello world
;; =>
;; [ _ "{whatever}" "* hello world" ]

(def front-matter-and-body-regex
  (let [any-string [:* [:alt :any :line-break]]           ; ".*" -- but the dot here doesn't match newlines, so we have to take care of this
        front-matter-edn  any-string
        body              any-string
        ws [:* :whitespace]                               ; I don't want it to break because of stray spaces in the front matter
        wn [:* [:alt :whitespace :line-break]]]
    (regal/regex [:cat
                  :start
                  wn     "#+name:" ws "front-matter"     ws :line-break
                  wn     "#+begin_src" ws "clojure"      ws :line-break
                  [:capture front-matter-edn]               :line-break
                  ws     "#+end_src"                     ws :line-break
                  [:capture body]])))

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
