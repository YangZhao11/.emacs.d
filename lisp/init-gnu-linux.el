; -*- coding: utf-8 -*-

;; --------------------------------------------------
;; google specific settings
;; needed for self-compiled emacs.
(defvar google-exclude-load-path-items
  '("emacs-google-config/third_party/elisp/auto_complete"
    "emacs-google-config/third_party/elisp/cl_lib"
    "emacs-google-config/third_party/elisp/company_mode"
    "emacs-google-config/third_party/elisp/dash"
    "emacs-google-config/third_party/elisp/ess_mode"
    "emacs-google-config/third_party/elisp/ess_mode/lisp"
    "emacs-google-config/third_party/elisp/f"
    "emacs-google-config/third_party/elisp/git_modes"
    "emacs-google-config/third_party/elisp/magit"
    "emacs-google-config/third_party/elisp/markdown_mode"
    "emacs-google-config/third_party/elisp/yasnippet"))

(defun allowed-google-load-path-additions (path)
  "Decides which PATH items should be added to the load path."
  (let ((value t))
    (dolist (exclude-suffix google-exclude-load-path-items value)
      (when (string-match exclude-suffix path)
        (setq value nil)))))

(setq google-update-load-path 'allowed-google-load-path-additions)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-google-config/devtools/editors/emacs")
(require 'google nil t)
(setq p4-use-p4config-exclusively t)
(require 'google-imports)
(require 'google-browse)
(require 'google-go)
(require 'google-cc-extras)

;; code search
(global-set-key (kbd "C-x C-.") 'codesearch-search)
(global-set-key (kbd "C-x C-,") 'google-open-in-code-search)

(when (fboundp 'autogen)
  (add-hook 'find-file-not-found-hooks 'autogen))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(set-frame-font "Menlo-11" 't 't)
(add-to-list 'auto-mode-alist '(".pipertmp-.*-change" . outline-mode))
