; -*- coding: utf-8 -*-

;; --------------------------------------------------
;; google specific settings
;; needed for self-compiled emacs.
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

;; fix for cases where we do not have autoloads defined
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))
