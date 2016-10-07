; -*- coding: utf-8 -*-

;; Make elpa packages available
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(setq package-archive-priorities
      '(("melpa-stable" . 20) ("gnu" . 10) ("melpa" . 0)))

(eval-when-compile (require 'use-package))
(require 'hydra)
(require 'diminish)
(require 's)

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
(setq inhibit-startup-screen t
      visible-bell 't
      set-mark-command-repeat-pop 't
      sentence-end-double-space nil
      require-final-newline 't
      tramp-default-method "ssh"
      text-scale-mode-step 1.1
      scroll-margin 3
      shift-select-mode nil
      mouse-yank-at-point 't
      default-input-method 'TeX)

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
(setq show-paren-when-point-in-periphery 't)
(electric-pair-mode 1)

;; --------------------------------------------------
(load-theme 'zenburn 't)   ; load this first for many defaults.

;; --------------------------------------------------
;; Load subfiles

(load "init-buffer")
(load "init-mode")
(load "init-key")

(setq-default mode-line-mule-info
              `("" . ,(cddr (default-value 'mode-line-mule-info))))
(setq-default mode-line-format
      `("%e" (:eval z-god-mode-lighter) .
        ,(cdr (default-value 'mode-line-format))))

(use-package server :diminish (server-buffer-clients . " #")
  :config (add-hook 'after-init-hook 'server-start))

(use-package edit-server :ensure
  :diminish (edit-server-edit-mode . " 🌍")
  :config
  (setq edit-server-new-frame nil
        edit-server-url-major-mode-alist
        '(("mail\\.google\\.com" . html-mode)
          ("snippets\\.googleplex\\.com" . markdown-mode)))
  (add-hook 'after-init-hook 'edit-server-start))

(setcdr (assq 'defining-kbd-macro minor-mode-alist)
        '((:propertize " ●" face (:foreground "#D04020")
                       help-echo "Recording keyboard macro")))
(setcdr (assq 'isearch-mode minor-mode-alist)
        '((:eval (if isearch-forward " ⇉" " ⇇"))))
(diminish 'next-error-follow-minor-mode " ⇅")
(setq overlay-arrow-string "➡")

;; line wrap symbol under terminal
(set-display-table-slot standard-display-table 'truncation ?↔)
(set-display-table-slot standard-display-table 'wrap ?↵)
(set-display-table-slot standard-display-table 'selective-display [?…])
(set-display-table-slot standard-display-table 'vertical-border ?│)


;; `Narrow' in mode line changed to §
(setq mode-line-modes
      (mapcar (lambda (x)
                (if (and (stringp x) (string= x "%n"))
                    `(:propertize (:eval (if (buffer-narrowed-p) " §"))
			help-echo "mouse-2: Remove narrowing from buffer"
			mouse-face mode-line-highlight
			local-map ,(make-mode-line-mouse-map
                                    'mouse-2 #'mode-line-widen))
                  x))
              mode-line-modes))

;; --------------------------------------------------
;; customs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("316d29f8cd6ca980bf2e3f1c44d3a64c1a20ac5f825a167f76e5c619b4e92ff4" "708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" "db9feb330fd7cb170b01b8c3c6ecdc5179fc321f1a4824da6c53609b033b2810" "2162da67ce86c514aff010de1b040fb26663ca42afebc2de26515d741121c435" "6916fa929b497ab630e23f2a4785b3b72ce9877640ae52088c65c00f8897d67f" "f21caace402180ab3dc5157d2bb843c4daafbe64aadc362c9f4558ac17ce43a2" "e1551b5516e0a439b6ab019ba00cee866e735f66f22ff67a5d882ad0f1383454" "01c5ebefcabc983c907ee30e429225337d0b4556cc1d21df0330d337275facbb" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "d1a42ed39a15a843cccadf107ee0242b5f78bfbb5b70ba3ce19f3ea9fda8f52d" "b6db49cec08652adf1ff2341ce32c7303be313b0de38c621676122f255ee46db" "50e7f9d112e821e42bd2b8410d50de966c35c7434dec12ddea99cb05dd368dd8" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" "e254f8e18ba82e55572c5e18f3ac9c2bd6728a7e500f6cc216e0c6f6f8ea7003" "d69a0f6d860eeff5ca5f229d0373690782a99aee2410a3eed8a31332a7101f1e" "e24679edfdea016519c0e2d4a5e57157a11f928b7ef4361d00c23a7fe54b8e01" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "03e3e79fb2b344e41a7df897818b7969ca51a15a67dc0c30ebbdeb9ea2cd4492" "0b6645497e51d80eda1d337d6cabe31814d6c381e69491931a688836c16137ed" "cdfb22711f64d0e665f40b2607879fcf2607764b2b70d672ddaa26d2da13049f" "7ed6913f96c43796aa524e9ae506b0a3a50bfca061eed73b66766d14adfa86d1" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "cdf488af2fbc0735c3eeff42e77bc62cb14bd869a89c6a27a854e2c4a50c9ad2" "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" "9bae7be09c7eba31130778f79b25ab5dc0fcf2af30588a7400343d99da3186e4" "ae1b8172198902655489ef133403e55cbfc2aff8147a9b7698196c4cb9bce8da" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "d6e27431f8cafb4a9136aebb1d4061f895b120bf88d34ff60c390d9905bd4e36" "86f4407f65d848ccdbbbf7384de75ba320d26ccecd719d50239f2c36bec18628" "1ef7df153ee59ef210acf0060073cd98e4992c9014b4fc7766243a3cb56cc6e4" "769bb56fb9fd7e73459dcdbbfbae1f13e734cdde3cf82f06a067439568cdaa95" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "dc77fb4e02417a6a932599168bd61927f6f2fe4fe3dbc4e4086a0bfb25babd67" "572caef0c27b100a404db8d540fd5b31397f90ab660ef5539ff0863ff9bee26a" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" default)))
 '(default-input-method "TeX")
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (ace-link link-hint counsel imenu-anywhere wgrep ivy-hydra flx swiper hydra keyfreq color-identifiers-mode org-bullets js2-mode loccur scala-mode2 clang-format markdown-mode use-package magit flycheck-status-emoji flycheck go-eldoc company company-go smex beacon anchored-transpose avy csv-mode go-mode easy-kill zenburn-theme yasnippet squery register-channel rainbow-delimiters lua-mode ido-vertical-mode ido-ubiquitous haskell-mode goto-chg god-mode f ess edit-server browse-kill-ring ace-window)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-template-field ((t (:background "#4d5d6d"))))
 '(font-lock-builtin-face ((t (:foreground "#C0C8D0" :weight normal))))
 '(font-lock-constant-face ((t (:foreground "#BFDBBF"))))
 '(font-lock-keyword-face ((t (:foreground "#E4E0BC" :weight normal))))
 '(font-lock-type-face ((t (:foreground "#90B8BC"))))
 '(link ((t (:foreground "#D0BF8F" :underline t :weight normal))))
 '(org-todo ((t (:foreground "#CC9393" :underline t :weight bold)))))
