#!/usr/bin/env -S emacs --quick --script

;; (princ (expand-file-name invocation-name invocation-directory))    ; the path of emacs executable
;; (princ (version))
;; (princ (org-version))


;; taken from https://gnu.support/gnu-emacs/emacs-lisp/Emacs-Lisp-emacs-org-to-html-el-on-command-line-convert-your-Org-files-on-command-line-to-HTML-output.html
(defun org-stdin-to-html-body-only ()
  "Reads org text body from STDIN and export full only body HTML"
  (let ((org-document-content "")
        this-read)
    (while (setq this-read (ignore-errors (read-from-minibuffer "")))
      (setq org-document-content (concat org-document-content "\n" this-read)))
    (with-temp-buffer
      (org-mode)
      (insert org-document-content)
      (org-html-export-as-html nil nil nil t)
      (princ (buffer-string)))))

(org-stdin-to-html-body-only)

