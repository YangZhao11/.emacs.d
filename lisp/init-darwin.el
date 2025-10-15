; -*- lexical-binding: t; coding: utf-8 -*-

(set-frame-font "JuliaMonoForced Nerd Font-17" nil 't)
;; (set-fontset-font t 'unicode
;;                   (font-spec :name "Apple Color Emoji" :size 11) nil 'prepend)

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("|>" "::" "->" "=>"))
  (ligature-set-ligatures 'help-mode '("=>"))
  (ligature-set-ligatures '(inferior-ess-mode ess-r-mode) '("|>" "::"))
  (ligature-set-ligatures '(julia-repl-mode julia-vterm-mode)
                          '("|>" "::" "->" "=>" "<|"))
  (global-ligature-mode t))

(setq ns-use-thin-smoothing t)          ; does not seem to have any effect
(setq mac-option-modifier 'none
      mac-command-modifier 'meta
      mac-right-option-modifier 'super
      mac-right-command-modifier 'control)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/git/current/bin")
(add-to-list 'exec-path "/opt/homebrew/sbin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "~/bin")

;; Fix environment variables
(setenv "PATH" (concat "~/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/git/current/bin:/usr/local/bin:"
                       (getenv "PATH")))
(setenv "LANG" "en_US.UTF-8")
;(setenv "RIPGREP_CONFIG_PATH" (concat (getenv "HOME") "/.config/ripgrep.conf"))
(cd "~")

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))
;; (setq ns-use-proxy-icon nil)

;; disable native-comp warnings
(setq comp-async-report-warnings-errors nil)

;; should work automatically but does not
(setq insert-directory-program "gls")

(defun z-after-init-darwin ()
  (when window-system
    (set-frame-size (selected-frame) 80 46)))

(add-hook 'after-init-hook 'z-after-init-darwin)
