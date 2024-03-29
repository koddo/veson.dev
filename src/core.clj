(ns core
  "https://veson.dev/blog -- statically generated"
  (:require [stasis.core :as stasis])
  (:require [clojure.java.shell :as sh])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as s])
  (:require [clojure.tools.reader.edn :as edn])
  (:require [lambdaisland.regal :as regal])
  (:require [nrepl.server :as nrepl-server]
            [cider.nrepl :refer (cider-nrepl-handler)])
  (:require [net.cgrand.enlive-html :as e])
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
      (let [front-matter (edn/read-string front-matter-edn)
            body-html (convert-org-str-to-html-str body-org)
            html (post-template (:title front-matter) body-html)]
       (assoc p
             :filename-export (s/replace filename #"\.org$" ".html")
             :front-matter front-matter
             :body-html body-html
             :html html
             ))
      ))
  
  

  (e/deftemplate post-template "post.html" [title body]
    [:head :title] (e/content title)
    [:#replace-me-with-actual-content] (e/substitute (e/html-snippet body)))
  ;; (print (apply str (post-template "my title" "body")))



  (defn final-pipeline []
    (let [all-pages (pages-unprocessed (stasis/slurp-directory "blog" #".*\.org$"))
          broken-pages (for [p all-pages
                             :when (nil? (:front-matter-edn p))]
                         p)]
      (if-not (empty? broken-pages)
        (str "Can't recognize front matter in these files: " (seq broken-pages))
        (process-pages all-pages)
        ))
    )


  (print (apply str (:html (first
                            (final-pipeline)q
                            ))))


  (defn stasis-compatible-map [l]
    (into {}
          (for [{:keys [filename-export html] :as e} l]
            {
             filename-export
             (apply str html)
             }
            ))
    )
  

  (let [target-dir "output"]
    (stasis/empty-directory! target-dir)
    (stasis/export-pages (stasis-compatible-map (final-pipeline)) target-dir))

  
  
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
  (let [     ; patterns below are similar to ".*" -- but the dot here doesn't match newlines, so we have to take care of this
        front-matter-edn  [:*? [:alt :any :line-break]]    ; lazily match, otherwise multiple front matters in a row match as one huge front matter
        body              [:*  [:alt :any :line-break]]    ; greedily here 
        ws [:* :whitespace]
        wn [:* [:alt :whitespace :line-break]]]    ; I don't want it to break because of stray spaces or newlines in the front matter
    (regal/regex [:cat
                  :start
                  wn     "#+name:" ws "front-matter"     ws :line-break
                  wn     "#+begin_src" ws "clojure"      ws :line-break
                  [:capture front-matter-edn]               :line-break
                  ws     "#+end_src"                     ws :line-break
                  [:capture body]])))

(defn convert-org-to-html-using-emacsclient [file-path]
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
     (convert-org-to-html-using-emacsclient temp-abspath)
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
  (println (nrepl-server/start-server
            ;; :port 7888
            :handler cider-nrepl-handler))
  (println "-main with" args)
  )
