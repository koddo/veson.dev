;; This library strips zero width space,
;; that acts as an escape character in org-mode, see https://orgmode.org/manual/Emphasis-and-Monospace.html
;; Org-mode doesn't do this by itself yet.
(add-to-list 'load-path (expand-file-name "org-extra-emphasis"))
(require 'org-extra-emphasis)


;; https://orgmode.org/manual/Publishing-options.html
(defun my-configuration ()
  (interactive)
  (setq publish-params
        (list
         :with-toc nil
         :section-numbers nil
         :html-doctype "html5"
         :html-html5-fancy t            ; for #+attr_html before #+begin ... #+end blocks and for exporting #+begin_aside as <aside>
         :html-container "article"      ; for top-level headings
         ))
  (setq body-only t))

(my-configuration)   ; this initializes the emacs server, and can be use to re-configure it through emacsclient if needed



(defun my-convert-org-to-html (file)   ; for executing via emacsclient
  (interactive)
  (find-file file)
  (org-html-export-to-html nil nil nil body-only publish-params)    ; this creates an html file with the same name as org file, simplifies the script significantly
  (kill-buffer))


