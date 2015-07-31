; -*- coding: utf-8 -*-

(use-package magit
  :commands (magit-status magit-get-top-dir)
  :config
  (when (boundp 'magit-auto-revert-mode) ; needed for magit < 2.0
    (diminish 'magit-auto-revert-mode)))

(use-package eldoc :diminish eldoc-mode
  :commands eldoc-mode)

(use-package bug-reference
  :commands bug-reference-mode
  :config
  (defun z-bug-to-link ()
    "Convert text captured from bug-reference-bug-regexp into links."
    (let ((m (match-string 2)))
      (if (s-ends-with? "@" m)
          (concat "teams/" (s-chop-suffix "@" m))
        (concat "http://" m))))
  (put 'z-bug-to-link 'bug-reference-url-format 't)
  (setq bug-reference-url-format 'z-bug-to-link
        bug-reference-bug-regexp
        "\\(\\b\\)\\(b/[0-9]+\\|c[rl]/[0-9]+\\|t/[0-9]+\\|\\(g\\|go\\|goto\\)/[-a-zA-z0-9_]+\\|[a-z]+@\\)"))

(use-package org
  :defer 't
  :bind (("<f5>" . org-capture) ("<f6>" . org-agenda))
  :config
  (setq org-speed-commands-user
        '(("S" . org-schedule) ("d" . org-deadline))
        org-todo-keywords
        '((sequence "PLAN(p)" "TODO(t)" "OPEN(o)" "WAIT(w)" "|" "OBSOLETE(e)" "DONE(d)"))
        org-todo-keyword-faces
        '(("OPEN" . "#90A8D0") ("PLAN" . "#A08880")
          ("OBSOLETE" . "#909090") ("WAIT" . "#CCA060"))
        org-use-speed-commands 't
        org-sparse-tree-default-date-type 'closed
        org-agenda-files '("~/Projects/NOTES.org")
        org-capture-templates
        '(("r" "Requests" entry
           (file+headline "~/Projects/NOTES.org" "Requests")
           "** TODO %t %?")
          ("n" "Notes" entry
           (file+headline "~/Projects/NOTES.org" "Captured Notes")
           "** %?\n%T"))
        org-link-abbrev-alist
        '(("doc" . "https://drive.google.com/drive/search?q=")
          ("ai" . "https://groups.google.com/a/google.com/forum/#!searchin/zhyang-ai/")))
  (define-key org-mode-map (kbd "M-m") nil)
  (defun z-org-mode-hook ()
    (bug-reference-mode)
    (setq-local register-channel-move-by-default 't))
  (add-hook 'org-mode-hook 'z-org-mode-hook))

;; --------------------------------------------------
;; eshell
(use-package eshell
  :commands eshell
  :config
  (defun z-eshell-prompt-function ()
    (let* ((pwd (eshell/pwd))
           (short-pwd (abbreviate-file-name pwd))
           (pwd-face 'default))
      (setq pwd-face
            (cond ((string= (nth 2 (file-attributes pwd 'string)) user-login-name)
                   'eshell-ls-symlink)
                  ((file-writable-p pwd) 'font-lock-constant-face)
                  ((file-readable-p pwd) 'eshell-ls-readonly)
                  (t 'eshell-ls-missing)))
      (concat
       (propertize short-pwd 'face pwd-face 'readonly t)
       (propertize (if (= (user-uid) 0) " # " " $ ")
                   'face 'eshell-prompt
                   'readonly t
                   'rear-nonsticky '(face readonly)))))
  (defalias 'eshell/x 'eshell/exit)
  (defun z-eshell-mode-hook ()
    (setq pcomplete-cycle-completions nil))
  (setq eshell-prompt-function 'z-eshell-prompt-function
        eshell-highlight-prompt nil)
  (add-hook 'eshell-mode-hook 'z-eshell-mode-hook))


;; --------------------------------------------------
;; modes

(defun z-c-mode-common-hook ()
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map (kbd "C-x t d") 'doxymacs-mode)
  (define-key c-mode-base-map (kbd "C-x t m") 'cwarn-mode)
  (setq c-electric-pound-behavior '(alignleft)) ;make a #define left-aligned
)
(add-hook 'c-mode-common-hook 'z-c-mode-common-hook)
(add-hook 'text-mode-hook 'auto-fill-mode)

(defun z-go-mode-hook ()
  (setq tab-width 4))
(add-hook 'go-mode-hook 'z-go-mode-hook)


(defun z-scala-mode-hook ()
  (setq prettify-symbols-alist
        (append '(("=>" . ?⇒)
                  ("->" . ?→))
                prettify-symbols-alist))
  (prettify-symbols-mode))
(add-hook 'scala-mode-hook 'z-scala-mode-hook)

(defun z-haskell-mode-hook ()
  (haskell-indentation-mode))
  ;;(setq haskell-font-lock-symbols 't)
(add-hook 'haskell-mode-hook 'z-haskell-mode-hook)

;; --------------------------------------------------
;; ess

(defun z-ess-mode-hook ()
  (define-key ess-mode-map [f7] 'ess-show-traceback)
  (define-key ess-mode-map [f8] 'ess-debug-next-or-eval-line)
  (define-key ess-mode-map [f9]
    'ess-eval-function-or-paragraph-and-step)
  (define-key ess-mode-map (kbd "C-x <f8>") 'ess-tracebug)
  (define-key ess-mode-map (kbd "\\") 'ess-smart-pipe)
  (define-key ess-mode-map (kbd ";") 'ess-smart-S-assign)
  (setq ess-tab-complete-in-script t)
  (rainbow-delimiters-mode 1)
  (z-ess-mode-symbols))

(defun ess-smart-pipe ()
  "Similar to ess-smart-S-assign, but insert %>% instead."
  (interactive)
  (let ((ess-S-assign " %>% ")
        (ess-smart-S-assign-key "\\"))
    (ess-smart-S-assign)))
(defun ess-smart-tpipe ()
  "Similar to ess-smart-S-assign, but insert %T>% instead."
  (interactive)
  (let ((ess-S-assign " %T>% ")
        (ess-smart-S-assign-key "?"))
    (ess-smart-S-assign)))

(defun ess-debug-next-or-eval-line ()
  (interactive)
  (let ((proc (and ess-current-process-name
                   (get-process ess-current-process-name))))
    (if (and proc
             (or (process-get proc 'dbg-active)
                 (process-get proc 'is-recover)))
        (ess-debug-command-next)
      (ess-eval-line-and-step))))

(defun z-ess-mode-symbols ()
  (when (fboundp 'prettify-symbols-mode) ; 24.4 needed
    (setq prettify-symbols-alist
          (append '(("%>%" . ?↦)
                    ("%T>%" . ?↧) ;↴
                    ("%<>%" . ?⇄) ;⇋⇌⇆
                    ;("%$%" . ?⊙)
                    ("%+%" . ?⊕)        ; ggplot2 has this
                    ("<=" . ?≤)
                    (">=" . ?≥)
                    ("%in%" . ?∈)
                    ("%*%" . ?×)
                    ("function" ?ƒ))
                  prettify-symbols-alist))
    (prettify-symbols-mode)))

(defun z-ess-help-mode-hook ()
  (define-key ess-help-mode-map [f8] 'ess-eval-line-and-step)
  (define-key ess-help-mode-map [f9]
    'ess-eval-function-or-paragraph-and-step)
  (z-ess-mode-symbols))

(defun z-inferior-ess-mode-hook ()
  (define-key inferior-ess-mode-map "\C-cw"
    'ess-execute-screen-options)
  (define-key inferior-ess-mode-map [f7] 'ess-show-R-traceback)
  (define-key inferior-ess-mode-map (kbd "C-x <f8>") 'ess-tracebug)
  (define-key inferior-ess-mode-map (kbd "\\") 'ess-smart-pipe)
  (define-key inferior-ess-mode-map (kbd ";") 'ess-smart-S-assign)
  (z-ess-mode-symbols))

(use-package ess-site
  :commands (R R-mode julia)
  :mode ((".R$" . r-mode) (".jl$" . julia-mode))
  :config
  (setq inferior-julia-program-name "~/bin/julia"
        ess-smart-S-assign-key ";")
  (ess-toggle-S-assign nil)
  (ess-toggle-S-assign nil)
  (ess-toggle-underscore nil)

  ;; In ESS-R, `$' is by default part of the symbol (_), which makes dabbrev
  ;; ignore variable names after $ for expansion. Fix by making it
  ;; punctuation.
  (modify-syntax-entry ?$ "." R-syntax-table)
  (add-hook 'ess-mode-hook 'z-ess-mode-hook)
  (add-hook 'ess-help-mode-hook 'z-ess-help-mode-hook)
  (add-hook 'inferior-ess-mode-hook 'z-inferior-ess-mode-hook))

;; Remove ## as begining of comment. Google R style guide insists we use
;; single #.
(defun z-remove-fancy-comments ()
  (interactive)
  (when (eq major-mode 'ess-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\( *\\)## " nil t)
        (replace-match "\\1# " nil nil)))))
(add-hook 'before-save-hook 'z-remove-fancy-comments)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package poly-R :ensure polymode
  :mode ("\\.Rmd\\'" . poly-markdown+r-mode))

(defun z-gdb-mode-hook () (setq gdb-many-windows t))
(add-hook 'gdb-mode-hook 'z-gdb-mode-hook)
