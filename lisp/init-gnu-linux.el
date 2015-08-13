; -*- coding: utf-8 -*-

;; --------------------------------------------------
;; google specific settings
;; needed for self-compiled emacs.
(defvar google-exclude-load-path-items
  '("google/third_party//auto_complete"
    "google/third_party//cl_lib"
    "google/third_party//company_mode"
    "google/third_party//dash"
    "google/third_party//ess_mode"
    "google/third_party//ess_mode/lisp"
    "google/third_party//f"
    "google/third_party//git_modes"
    "google/third_party//magit"
    "google/third_party//markdown_mode"
    "google/third_party//yasnippet"))

(defun allowed-google-load-path-additions (path)
  "Decides which PATH items should be added to the load path."
  (let ((value t))
    (dolist (exclude-suffix google-exclude-load-path-items value)
      (when (string-match exclude-suffix path)
        (setq value nil)))))

(setq google-update-load-path 'allowed-google-load-path-additions)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/google")
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

;; ;; fix for cases where we do not have autoloads defined
;; (use-package markdown-mode
;;   :mode ("\\.md\\'" . markdown-mode))
