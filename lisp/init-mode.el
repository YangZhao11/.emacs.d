; -*- coding: utf-8 -*-

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq with-editor-mode-lighter "")
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package eldoc :diminish eldoc-mode
  :commands eldoc-mode)

(use-package imenu
  :bind ("C-x j" . imenu))

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

;; --------------------------------------------------
(use-package yasnippet :demand ;; :ensure
  :diminish yas-minor-mode
  :bind ("M-?" . yas-insert-snippet)
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (setq yas-prompt-functions
        '(yas-ido-prompt yas-completing-prompt yas-no-prompt)
        yas-wrap-around-region t)
  (yas-global-mode))

(defun z-re-backward (re count)
  "Search RE backward, return COUNT submatch.  Used in snippets."
  (save-excursion
    (save-match-data
      (when (re-search-backward re (point-min) t)
          (match-string count)))))

(use-package company :defer 't
  :diminish " ♿"
  :bind (("C-x t m" . company-mode)
         ("M-m" . company-complete))
  :config
  (setq company-idle-delay nil))

(use-package flycheck
  :bind ("C-x t k" . flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-status-emoji-mode))

;; ----------------------------------------
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
           "** %? %T"))
        org-link-abbrev-alist
        '(("doc" . "https://drive.google.com/drive/search?q=")
          ("ai" . "https://groups.google.com/a/google.com/forum/#!searchin/zhyang-ai/")))
  (define-key org-mode-map (kbd "M-m") nil)
  (defun z-org-mode-hook ()
    (bug-reference-mode)
    (setq-local register-channel-move-by-default 't)
    (setq-local ido-use-filename-at-point nil))
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
  (defalias 'eshell/l 'eshell/ls)
  (defalias 'eshell/p 'find-file-read-only)
  (defalias 'eshell/ec 'find-file)
  (defun z-eshell-mode-hook ()
    (setq pcomplete-cycle-completions nil))
  (setq eshell-prompt-function 'z-eshell-prompt-function
        eshell-highlight-prompt nil)
  (add-hook 'eshell-mode-hook 'z-eshell-mode-hook))


;; --------------------------------------------------
;; modes

(defun z-c-mode-common-hook ()
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (setq c-electric-pound-behavior '(alignleft)) ;make a #define left-aligned
)
(add-hook 'c-mode-common-hook 'z-c-mode-common-hook)

(defun z-c++-mode-hook ()
  (setq flycheck-clang-language-standard "c++14"
        flycheck-gcc-language-standard "c++14")
  (abbrev-mode -1)
  (require 'clang-format nil 't))
(add-hook 'c++-mode-hook 'z-c++-mode-hook)

(defun z-maybe-clang-format ()
  (when (eq major-mode 'c++-mode)
    (clang-format-buffer)))
(use-package clang-format
  :config
  (add-hook 'before-save-hook 'z-maybe-clang-format))

(defun z-go-mode-hook ()
  (setq tab-width 4)
  (setq-local company-backends '(company-go))
  (company-mode 1)
  (go-eldoc-setup))
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

(defun ess-render-markdown ()
  (interactive)
  (ess-eval-linewise (concat "render(\"" buffer-file-name "\", output_dir = getwd())") nil 'eob))

(defun z-ess-mode-symbols ()
  (setq prettify-symbols-alist
        (append '(("%>%" . ?↦)
                  ("%T>%" . ?↧) ;↴
                  ("%<>%" . ?⇄) ;⇋⇌⇆
                  ("<=" . ?≤)
                  (">=" . ?≥)
                  ("%in%" . ?∈)
                  ("%*%" . ?×)
                  ("function" ?ƒ))
                prettify-symbols-alist))
  (prettify-symbols-mode))

(defun z-ess-mode-hook ()
  (rainbow-delimiters-mode 1)
  (z-ess-mode-symbols)
  (setq-local company-backends ess-company-backends)
  (company-mode))

(use-package ess-site
  :commands (R R-mode julia)
  :mode ((".R$" . r-mode) (".Rmd$" . r-mode) (".jl$" . julia-mode))
  :config
  (setq inferior-julia-program-name "~/bin/julia"
        ess-tab-complete-in-script 't
        ess-smart-S-assign-key ";"
        ess-busy-strings '("  " " ◴" " ◷" " ◶" " ◵"))
  (ess-toggle-S-assign nil)
  (ess-toggle-S-assign nil)
  (ess-toggle-underscore nil)

  ;; In ESS-R, `$' is by default part of the symbol (_), which makes dabbrev
  ;; ignore variable names after $ for expansion. Fix by making it
  ;; punctuation.
  (modify-syntax-entry ?$ "." R-syntax-table)

  ;; For Rmd editing, do not treat ` as quote.
  (modify-syntax-entry ?` "." R-syntax-table)

  (bind-keys :map ess-mode-map
             ("<f7>" . ess-show-traceback)
             ("<f8>" . ess-debug-next-or-eval-line)
             ("<f9>" . ess-eval-function-or-paragraph-and-step)
             ("C-x <f8>" . ess-tracebug)
             ("C-c SPC" . ess-render-markdown)
             ("\\" . ess-smart-pipe)
             (";" . ess-smart-S-assign))

  (bind-keys :map inferior-ess-mode-map
             ("\C-cw" . ess-execute-screen-options)
             ("<f7>" . ess-show-R-traceback)
             ("C-x <f8>" . ess-tracebug)
             ("\\" . ess-smart-pipe)
             (";" .  ess-smart-S-assign))

  (bind-keys :map ess-help-mode-map
             ("<f8>" . ess-eval-line-and-step)
             ("<f9>" . ess-eval-function-or-paragraph-and-step))

  (add-hook 'ess-mode-hook 'z-ess-mode-hook)
  (add-hook 'ess-help-mode-hook 'z-ess-mode-symbols)
  (add-hook 'inferior-ess-mode-hook 'z-ess-mode-symbols))

;; Remove ## as begining of comment. Google R style guide insists we use
;; single #.
(defun z-remove-fancy-comments ()
  (interactive)
  (when (eq major-mode 'ess-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\( *\\)## " nil t)
        (replace-match "\\1# " nil nil)))))
;; (add-hook 'before-save-hook 'z-remove-fancy-comments)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun z-gdb-mode-hook () (setq gdb-many-windows t))
(add-hook 'gdb-mode-hook 'z-gdb-mode-hook)
