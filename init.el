; -*- coding: utf-8 -*-
(setq inhibit-startup-screen t)
(server-start)

;; Make elpa packages available
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(eval-when-compile (require 'use-package))

;; Load platform specific inits, including init-gnu-linux /
;; init-darwin, init-ns / init-x, init-hostname.
(push "~/.emacs.d/lisp" load-path)
(dolist (sub (list
              (replace-regexp-in-string "/" "-" (symbol-name system-type))
              (symbol-name window-system)
              (replace-regexp-in-string "\\..*" "" system-name)))
  (load (concat "init-" sub) t))

;; --------------------------------------------------
;; Defaults
(setq-default indent-tabs-mode nil
              indicate-empty-lines 't
              frame-title-format "%b @Emacs"
              ispell-program-name "aspell"
              page-delimiter "\\(^\f\\|-\\{5,\\}$\\)")
(setq visible-bell 't
      set-mark-command-repeat-pop 't
      sentence-end-double-space nil
      require-final-newline 't
      tramp-default-method "ssh"
      text-scale-mode-step 1.1)

(menu-bar-mode (if (eq system-type 'darwin) 1 -1)) ; Mac always has menu bar
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(mouse-wheel-mode 1)
(blink-cursor-mode 1)
(transient-mark-mode 1)
(global-font-lock-mode 't)
(ansi-color-for-comint-mode-on)
(setq comint-scroll-to-bottom-on-output 't
      comint-scroll-show-maximum-output nil)
(temp-buffer-resize-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(electric-pair-mode)

;; --------------------------------------------------
(require 's)
(load-theme 'zenburn t)

;; --------------------------------------------------
(use-package yasnippet
  :demand
  :diminish yas-minor-mode
  :bind ("M-?" . yas-insert-snippet)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")
        yas-prompt-functions
        '(yas-ido-prompt yas-completing-prompt yas-no-prompt)
        yas-wrap-around-region t)
  (yas-global-mode))

(defun z-re-backward (re count)
  "Search re backward, returns count-th submatch. Used in snippets."
  (save-excursion
    (save-match-data
      (save-restriction  ; Needed for yasnippet 0.8. Remove after 0.9.
        (widen)
        (when (re-search-backward re (point-min) t)
          (match-string count))))))

;; --------------------------------------------------
;; Load subfiles

(load "init-buffer")
(load "init-mode")
(load "init-key")

(setq magit-last-seen-setup-instructions "1.4.0")
;; --------------------------------------------------
;; customs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7ed6913f96c43796aa524e9ae506b0a3a50bfca061eed73b66766d14adfa86d1" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "cdf488af2fbc0735c3eeff42e77bc62cb14bd869a89c6a27a854e2c4a50c9ad2" "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" "9bae7be09c7eba31130778f79b25ab5dc0fcf2af30588a7400343d99da3186e4" "ae1b8172198902655489ef133403e55cbfc2aff8147a9b7698196c4cb9bce8da" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "d6e27431f8cafb4a9136aebb1d4061f895b120bf88d34ff60c390d9905bd4e36" "86f4407f65d848ccdbbbf7384de75ba320d26ccecd719d50239f2c36bec18628" "1ef7df153ee59ef210acf0060073cd98e4992c9014b4fc7766243a3cb56cc6e4" "769bb56fb9fd7e73459dcdbbfbae1f13e734cdde3cf82f06a067439568cdaa95" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "dc77fb4e02417a6a932599168bd61927f6f2fe4fe3dbc4e4086a0bfb25babd67" "572caef0c27b100a404db8d540fd5b31397f90ab660ef5539ff0863ff9bee26a" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" default))))

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(link ((t (:foreground "#D0BF8F" :underline t :weight normal)))))
