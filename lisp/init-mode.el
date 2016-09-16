; -*- coding: utf-8 -*-

(eval-when-compile
  (require 'use-package)
  (require 'hydra))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'text-mode-hook #'abbrev-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

(use-package dired
  :config
  (defhydra hydra-dired (:color teal :columns 3)
    "Action [available directly]"
    ("!" dired-do-shell-command "shell")
    ("#" dired-flag-auto-save-files "mark auto-saves")
    ("$" dired-hide-subdir "hide subdir")
    ("%" hydra-dired-regex/body "regex")
    ("&" dired-do-async-shell-command "async-shell")
    ("(" dired-hide-details-mode "hide details")
    ("*" hydra-dired-mark/body "mark")
    ("=" dired-diff "diff")
    ("A" dired-do-find-regexp "find-regexp")
    ("B" dired-do-byte-compile "byte-compile")
    ("C" dired-do-copy "copy")
    ("D" dired-do-delete "delete")
    ("F" dired-do-find-marked-files "find-marked-files")
    ("G" dired-do-chgrp "chgrp")
    ("H" dired-do-hardlink "hardlink")
    ("I" dired-info "info")
    ("L" dired-do-load "load")
    ("M" dired-do-chmod "chmod")
    ("N" dired-man "man")
    ("O" dired-do-chown "chown")
    ("P" dired-do-print "print")
    ("Q" dired-do-find-regexp-and-replace "replace regexp")
    ("R" dired-do-rename "rename")
    ("S" dired-do-symlink "symlink")
    ("T" dired-do-touch "touch")
    ("U" dired-unmark-all-marks "unmark all")
    ("Y" dired-do-relsymlink "relsymlink")
    ("Z" dired-do-compress "compress")
    ("^" dired-up-directory "up-directory")
    ("a" dired-find-alternate-file "find-alternate-file")
    ("c" dired-do-compress-to "compress-to")
    ("d" dired-flag-file-deletion "flag-file-deletion")
    ("i" dired-maybe-insert-subdir "maybe-insert-subdir")
    ("j" dired-goto-file "goto-file")
    ("k" dired-do-kill-lines "kill-lines")
    ("l" dired-do-redisplay "redisplay")
    ("o" dired-find-file-other-window "find-file-other-window")
    ("s" dired-sort-toggle-or-edit "sort-toggle-or-edit")
    ("v" dired-view-file "view-file")
    ("w" dired-copy-filename-as-kill "copy-filename-as-kill")
    ("x" dired-do-flagged-delete "flagged-delete")
    ("y" dired-show-file-type "show-file-type")
    ("~" dired-flag-backup-files "flag-backup-files"))

  (defhydra hydra-dired-mark (:color teal :columns 3)
    "Mark"
    ("!" dired-unmark-all-marks  "unmark all")
    ("%" dired-mark-files-regexp "regexp")
    ("(" dired-mark-sexp         "sexp")
    ("*" dired-mark-executables  "executables")
    ("." dired-mark-extension    "extension")
    ("/" dired-mark-directories  "directories")
    ("?" dired-unmark-all-files  "unmark specific")
    ("@" dired-mark-symlinks     "symlinks")
    ("O" dired-mark-omitted      "omitted")
    ("c" dired-change-marks      "change")
    ("s" dired-mark-subdir-files "subdir-files"))

  (defhydra hydra-dired-regex (:color teal :columns 3)
    "Regex"
    ("&" dired-flag-garbage-files "flag-garbage-files")
    ("C" dired-do-copy-regexp "copy")
    ("H" dired-do-hardlink-regexp "hardlink")
    ("S" dired-do-symlink-regexp "symlink")
    ("Y" dired-do-relsymlink-regexp "relsymlink")
    ("d" dired-flag-files-regexp "flag-files")
    ("g" dired-mark-files-containing-regexp "mark-containing")
    ("l" dired-downcase "downcase")
    ("m" dired-mark-files-regexp "mark")
    ("r" dired-do-rename-regexp "rename")
    ("u" dired-upcase "upcase"))

  (bind-keys :map dired-mode-map
             ("SPC" . hydra-dired/body)
             ("*" . hydra-dired-mark/body)
             ("%" . hydra-dired-regex/body)))

(defhydra hydra-smerge
  (:color red :hint nil
          :pre (smerge-mode 1))
    "
^Move^      ^Keep^         ^Diff^      ^Pair^
------------------------------------------------------
_n_ext      _b_ase         _R_efine    _<_: base-mine
_p_rev      _m_ine         _E_diff     _=_: mine-other
^ ^         _o_ther        _C_ombine   _>_: base-other
^ ^         _a_ll          _r_esolve
_q_uit      _RET_: current
"
    ("RET" smerge-keep-current)
    ("C"   smerge-combine-with-next)
    ("E"   smerge-ediff)
    ("R"   smerge-refine)
    ("a"   smerge-keep-all)
    ("b"   smerge-keep-base)
    ("m"   smerge-keep-mine)
    ("n"   smerge-next)
    ("o"   smerge-keep-other)
    ("p"   smerge-prev)
    ("r"   smerge-resolve)
    ("<"   smerge-diff-base-mine)
    ("="   smerge-diff-mine-other)
    (">"   smerge-diff-base-other)
    ("q"   nil :color blue))
(bind-key "C-x m" 'hydra-smerge/body)

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq with-editor-mode-lighter "")
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-completing-read-function 'ivy-completing-read))

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

(use-package color-identifiers-mode
  :diminish 'color-identifiers-mode
  :commands (color-identifiers-mode)
  :config
  (setq color-identifiers:min-color-saturation 0.1
        color-identifiers:max-color-saturation 0.3))

;; --------------------------------------------------
(use-package yasnippet :demand ;; :ensure
  :diminish yas-minor-mode
  :bind ("M-?" . yas-insert-snippet)
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (defun yas-ivy-prompt (prompt choices &optional display-fn)
    (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))
  (setq yas-prompt-functions
        '(yas-ivy-prompt yas-completing-prompt yas-no-prompt)
        yas-wrap-around-region t)
  (yas-global-mode))

(defun z-re-backward (re count)
  "Search RE backward, return COUNT submatch.  Used in snippets."
  (save-excursion
    (save-match-data
      (when (re-search-backward re (point-min) t)
          (match-string count)))))

(use-package company :defer 't
  :diminish " ▤"
  :commands (company-mode)
  :bind (("M-m" . company-complete))
  :config
  (setq company-idle-delay nil))

(use-package flycheck
  :commands (flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-status-emoji-mode))

(use-package ycmd :diminish " ☯" :no-require t
  :defines ycmd-server-command ycmd-global-config
  ycmd-extra-conf-whitelist ycmd-idle-change-delay
  :commands (ycmd-mode)
  :config
  (defconst google-ycmd--extra-conf "/usr/lib/youcompleteme/ycm_extra_conf.py")
  (setq ycmd-server-command
        '("/usr/grte/v4/bin/python2.7"
          "/usr/lib/youcompleteme/third_party/ycmd/ycmd"))
  (setq ycmd-global-config google-ycmd--extra-conf)
  (add-to-list 'ycmd-extra-conf-whitelist google-ycmd--extra-conf)
  (setq ycmd-idle-change-delay 0.5))

;; ----------------------------------------
(use-package org
  :defer 't
  :bind (("<f5>" . org-capture) ("<f6>" . org-agenda))
  :config
  (defface org-todo-open '((t :foreground "#90A8D0" :inherit org-todo))
           "face for org mode OPEN keyword" :group 'org-faces)
  (defface org-todo-wait '((t :foreground "#CCA060" :inherit org-todo))
           "face for org mode WAIT keyword" :group 'org-faces)
  (defface org-done-obsolete '((t :foreground "#909090" :inherit org-done))
           "face for org mode OBSOLETE keyword" :group 'org-faces)
  (setq org-speed-commands-user
        '(("S" . org-schedule) ("d" . org-deadline))
        org-todo-keywords
        '((sequence "TODO(t)" "OPEN(o)" "WAIT(w)"
                    "|" "OBSOLETE(e)" "DONE(d)"))
        org-todo-keyword-faces
        '(("OPEN" . org-todo-open)
          ("OBSOLETE" . org-done-obsolete) ("WAIT" . org-todo-wait))
        org-use-speed-commands 't
        org-sparse-tree-default-date-type 'closed
        org-agenda-files '("~/Projects/notes/NOTES.org")
        org-refile-targets '((nil :level . 1) (nil :tag . "OLD"))
        org-capture-templates
        '(("r" "Requests" entry
           (file+headline "~/Projects/notes/NOTES.org" "Requests")
           "** TODO %t %?")
          ("n" "Notes" entry
           (file+headline "~/Projects/notes/NOTES.org" "Captured Notes")
           "** %? %T"))
        org-link-abbrev-alist
        '(("doc" . "https://drive.google.com/drive/search?q=")
          ("ai" . "https://groups.google.com/a/google.com/forum/#!searchin/zhyang-ai/")))
  (bind-keys :map org-mode-map ("M-m") ("C-j"))
  (defun z-org-mode-hook ()
    (bug-reference-mode 1)
    (org-bullets-mode 1)
    (setq-local register-channel-move-by-default 't)
    (setq-local ido-use-filename-at-point nil))
  (add-hook 'org-mode-hook 'z-org-mode-hook))

;; --------------------------------------------------
;; eshell
(use-package eshell
  :commands eshell
  :functions eshell/pwd
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

(use-package elisp-mode
  :config
  (defun z-setup-imenu-for-use-package ()
    "Recognize `use-package` in imenu"
    (let ((emacsd (expand-file-name "~/.emacs.d/lisp/"))
          (initel (expand-file-name "init.el" "~/.emacs.d")))
      (when (and buffer-file-name
                 (or (string= buffer-file-name initel)
                     (string-match (rx-to-string `(: bos ,emacsd) t)
                                   buffer-file-name)))
        (add-to-list
         'imenu-generic-expression
         '(nil "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)))))
  (add-hook 'emacs-lisp-mode-hook 'z-setup-imenu-for-use-package)
  (add-hook 'emacs-lisp-mode-hook 'color-identifiers-mode)
  (bind-keys :map lisp-interaction-mode-map ("C-j")))


(use-package cc-mode
  :config
  (setq c-electric-pound-behavior '(alignleft)) ;make a #define left-aligned
  (bind-key "RET" 'newline-and-indent c-mode-base-map))

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
  (setq-local company-backends '(company-ycmd))
  ;; (ycmd-mode 1)
  ;; (company-mode 1)
  ;;(go-eldoc-setup)
  )
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

(use-package ess-site
  :commands (R R-mode julia)
  :mode ((".R$" . r-mode) (".Rmd$" . r-mode) (".jl$" . julia-mode))
  :defines ess-company-backends ess-current-process-name
  :functions ess-smart-S-assign ess-debug-command-next
    ess-eval-line-and-step ess-eval-linewise ess-toggle-S-assign
    ess-toggle-underscore
  :config
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
    (ess-eval-linewise
     (concat "render(\"" buffer-file-name
             "\", output_dir = getwd())") nil 'eob))

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
    (when (string-match "\\.Rmd$" buffer-file-name)
      (setq-local page-delimiter "^```\\({.*}\\)?$"))
    (rainbow-delimiters-mode 1)
    (z-ess-mode-symbols)
    (setq-local company-backends ess-company-backends)
    (company-mode))

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
  (modify-syntax-entry ?$ "." ess-r-syntax-table)

  ;; For Rmd editing, do not treat ` as quote.
  (modify-syntax-entry ?` "." ess-r-syntax-table)

  (bind-keys :map ess-mode-map
             ("<f7>" . ess-show-traceback)
             ("<f8>" . ess-debug-next-or-eval-line)
             ("<f9>" . ess-eval-function-or-paragraph-and-step)
             ("C-x <f8>" . ess-tracebug)
             ("C-c SPC" . ess-render-markdown)
             ("C-c C-m" . markdown-mode)
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

(use-package markdown-mode
  :mode (".md$" . markdown-mode)
  :config
  (bind-keys :map markdown-mode-map
             ("C-c C-m" . r-mode)))

(use-package gdb-mi
  :config
  (defun z-gdb-mode-hook () (setq gdb-many-windows t))
  (add-hook 'gdb-mode-hook 'z-gdb-mode-hook))
