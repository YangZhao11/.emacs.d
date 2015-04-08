(global-set-key (kbd "C-`") (kbd "C-SPC"))
(global-set-key (kbd "C-;") (kbd "C-SPC"))
(global-set-key (kbd "C-x C-;") (kbd "C-x C-SPC"))
(global-set-key (kbd "C-x C-`") (kbd "C-x C-SPC"))
(global-set-key (kbd "C-M-y") 'browse-kill-ring)
(global-set-key (kbd "M-s M-o") 'multi-occur-in-matching-buffers)
;; occur-edit-mode in occur mode key binding is 'e'
(global-set-key (kbd "M-s g") 'grep)
(global-set-key (kbd "M-s M-g") 'rgrep)

(defun shrink-other-window-if-larger-than-buffer ()
    (interactive)
    "Shrink other window if larger than buffer"
    (shrink-window-if-larger-than-buffer
     (next-window (selected-window) nil nil)))
(global-set-key (kbd "C-x _") 'shrink-other-window-if-larger-than-buffer)
(global-set-key (kbd "C-x 9") 'delete-other-windows-vertically)

(global-set-key [f1] 'ispell-word)
(global-set-key (kbd "C-x <f1>") 'ispell-comments-and-strings)
(global-set-key [f2] 'eshell)
;; F3 and F4 for macros
;(global-set-key (kbd "C-x <f5>") 'rgrep)
(global-set-key [f7] 'gud-up)
(global-set-key [(shift f7)] 'gud-down)
(global-set-key [f8] 'gud-next)
(global-set-key [(shift f8)] 'gud-step)
(global-set-key [f9] 'gud-finish)

(defun toggle-one-window ()
  "Change to one window (C-x 1) if applicable, otherwise show
other buffer in other window."
  (interactive)
  (if (window-parent)
      (delete-other-windows)
    (display-buffer (other-buffer) t)))
(global-set-key [f10] 'toggle-one-window)
(global-set-key [f11] 'switch-to-prev-buffer)
(global-set-key [f12] 'switch-to-next-buffer)
(defun z-prev-buffer-next-window () (interactive)
  (switch-to-prev-buffer (next-window)))
(global-set-key [(shift f11)] 'z-prev-buffer-next-window)
(defun z-next-buffer-next-window () (interactive)
  (switch-to-next-buffer (next-window)))
(global-set-key [(shift f12)] 'z-next-buffer-next-window)

;; White space handling. White space mode is more comprehensive, but
;; has the annoying bug(in emacs 23.1) of showing trailing white space font for
;; spaces in front of the cursor.
(setq whitespace-style '(tabs lines-tail))
;(global-whitespace-mode t)
;(setq-default show-trailing-whitespace t)
(defun toggle-show-trailing-whitespace ()
   "Toggle show-trailing-whitespace"
   (interactive)
   (setq show-trailing-whitespace (not show-trailing-whitespace))
   (message "show-trailing-whitespace set to %s" show-trailing-whitespace))

;; Toggle commands
(global-unset-key (kbd "C-x t"))
(global-set-key (kbd "C-x t a") 'abbrev-mode)
(global-set-key (kbd "C-x t c") 'highlight-changes-mode)
(global-set-key (kbd "C-x t d") 'eldoc-mode)
(global-set-key (kbd "C-x t f") 'auto-fill-mode)
(global-set-key (kbd "C-x t h") 'hi-lock-mode)
(global-set-key (kbd "C-x t k") 'flyspell-prog-mode)
(global-set-key (kbd "C-x t l") 'flyspell-mode)
(global-set-key (kbd "C-x t n") 'linum-mode)
(global-set-key (kbd "C-x t o") 'outline-minor-mode)
(global-set-key (kbd "C-x t p") 'rainbow-delimiters-mode)
(global-set-key (kbd "C-x t r") 'hs-minor-mode)
(global-set-key (kbd "C-x t s") 'whitespace-mode)
(global-set-key (kbd "C-x t t") 'toggle-show-trailing-whitespace)
(global-set-key (kbd "C-x t v") 'view-mode)
(global-set-key (kbd "C-x t w") 'subword-mode)

(defun toggle-transient-mark-mode ()
  "Toggle transient mark mode temporarily."
  (interactive)
  (if transient-mark-mode (setq transient-mark-mode nil)
    (setq transient-mark-mode 'lambda)))
(global-set-key (kbd "M-i") 'toggle-transient-mark-mode)

(require 'goto-chg)
(global-set-key (kbd "C-.") 'goto-last-change)
(global-set-key (kbd "C-,") 'goto-last-change-reverse)

(require 'register-channel)
(register-channel-mode)

(defun all-frames-to-messages-buffer ()
  "make all frames display the *Messages* buffer only after
storing current frame configuration to register 9."
  (interactive)
  (frame-configuration-to-register ?9)
  (dolist (f (frame-list))
    (let ((w (frame-first-window f)))
      (delete-other-windows w)
      (set-window-buffer w "*Messages*"))))
(define-key register-channel-mode-map (kbd "M-g 9") 'all-frames-to-messages-buffer)
(define-key register-channel-mode-map (kbd "M-9") 'register-channel-dwim)

(require 'ace-jump-mode)
(global-set-key (kbd "C-j") 'ace-jump-mode)
(setq ace-jump-mode-scope 'frame)
(setq ace-jump-mode-gray-background nil)

(require 'ace-window)
(global-set-key (kbd "M-0") 'ace-window)
(setq aw-scope 'frame)
(setq aw-background nil)

(require 'ace-jump-zap)
(global-set-key (kbd "M-z") 'ace-jump-zap-up-to-char-dwim)
(global-set-key (kbd "M-Z") 'ace-jump-zap-to-char-dwim)

(require 'smartscan)
(global-smartscan-mode 1)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)

(require 'expand-region)
(global-set-key (kbd "C-\\") 'er/expand-region)
(global-set-key (kbd "C-|") 'er/contract-region)

(require 'multiple-cursors)
(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m ,") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "M-m M-m") 'mc/mark-all-dwim)
(global-set-key (kbd "M-m m") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "M-m /") 'mc/edit-lines)
(global-set-key (kbd "M-m n") 'mc/insert-numbers)
(global-set-key (kbd "M-m SPC") 'mc/mark-pop)
(global-set-key (kbd "M-m M-SPC") 'mc/mark-pop)
(global-set-key (kbd "M-m s") 'mc/sort-regions)
(global-set-key (kbd "M-m r") 'mc/reverse-regions)
(add-to-list 'mc/unsupported-minor-modes 'god-local-mode)

(require 'god-mode)
(global-set-key (kbd "ESC ESC") 'god-mode-all)
(define-key god-local-mode-map (kbd "z") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-mode-all)
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "ESC ESC") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "ESC ESC") 'god-mode-isearch-disable)

(dolist (i '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
  ;; directly bind these to commands, instead of making it a macro so
  ;; that messages work in god-mode.
  (global-set-key (kbd (concat "C-x C-" i)) (key-binding (kbd (concat "C-x " i))))
  (global-set-key (kbd (concat "M-g C-" i)) (key-binding (kbd (concat "M-g " i)))))

(add-to-list 'god-exempt-major-modes 'ibuffer-mode)
(defun z-god-mode-update-modline ()
  (cond (god-local-mode (set-face-background 'mode-line "blue4"))
        (t  (set-face-background 'mode-line "#2B2B2B"))))
(add-hook 'god-mode-enabled-hook 'z-god-mode-update-modline)
(add-hook 'god-mode-disabled-hook 'z-god-mode-update-modline)
