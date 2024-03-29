#!/usr/bin/env -S emacs --quick --script

;; (princ (expand-file-name invocation-name invocation-directory))    ; the path of emacs executable
;; (princ (version))
;; (princ (org-version))
;; (princ load-path)

;; (require 'org)

;; This library strips zero width space,
;; that acts as an escape character in org-mode, see https://orgmode.org/manual/Emphasis-and-Monospace.html
;; Org-mode doesn't do this by itself yet.
(add-to-list 'load-path (expand-file-name "org-extra-emphasis"))
(require 'org-extra-emphasis)


;; https://orgmode.org/manual/Publishing-options.html
(setq publish-params
      (list
       :with-toc nil
       :section-numbers nil
       :html-doctype "html5"
       :html-html5-fancy t            ; for #+attr_html before #+begin ... #+end blocks and for exporting #+begin_aside as <aside>
       :html-container "article"      ; for top-level headings
       ))

(setq body-only t)


(defun org-stdin-to-html (org-publish-options body-only)
  "Reads org text body from STDIN and export full only body HTML"
  (let ((org-document-content "")
        this-read)
    (while (setq this-read (ignore-errors (read-from-minibuffer "")))
      (setq org-document-content (concat org-document-content "\n" this-read)))
    (with-temp-buffer
      (org-mode)
      (insert org-document-content)
      (org-html-export-as-html nil nil nil body-only org-publish-options)
      (princ (buffer-string)))))


(defun run ()
  (interactive)
  (org-stdin-to-html publish-params body-only))


(run)

