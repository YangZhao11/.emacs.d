(global-set-key (kbd "M-w") 'easy-kill)
(global-set-key (kbd "C-M-y") 'browse-kill-ring)
(global-set-key (kbd "M-s M-o") 'multi-occur-in-matching-buffers)
;; occur-edit-mode in occur mode key binding is 'e'
(global-set-key (kbd "M-s g") 'grep)
(global-set-key (kbd "M-s M-g") 'rgrep)
(defun z-exchange-point-and-mark (&optional arg)
  "Like exchange-point-and-mark, but arg means toggle active
region, instead of inactivate region."
  (interactive "P")
  (let ((active (or (and arg (not (use-region-p)))
                    (and (not arg) (use-region-p)))))
    (exchange-point-and-mark (not active))))
(global-set-key (kbd "C-x C-x") 'z-exchange-point-and-mark)

(require 'dired-x)                      ;bind C-x C-j to dired-jump

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
(global-set-key [f11] 'shrink-window)
(global-set-key [f12] 'enlarge-window)

(global-set-key (kbd "M-9") 'switch-to-prev-buffer)
(global-set-key (kbd "M-0") 'switch-to-next-buffer)
(defun z-prev-buffer-next-window () (interactive)
  (switch-to-prev-buffer (next-window)))
(global-set-key (kbd "M-(") 'z-prev-buffer-next-window)
(defun z-next-buffer-next-window () (interactive)
  (switch-to-next-buffer (next-window)))
(global-set-key (kbd "M-)") 'z-next-buffer-next-window)

(defun isearch-exit-other-end (rbeg rend)
    "Exit isearch, but at the other end of the search string.
  This is useful when followed by an immediate kill."
    (interactive "r")
    (isearch-exit)
    (goto-char isearch-other-end))
(define-key isearch-mode-map (kbd "M-RET") 'isearch-exit-other-end)

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
(global-set-key (kbd "C-x t W") 'superword-mode)

(defun z-toggle-activate-mark ()
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
      (activate-mark)))
(global-set-key (kbd "M-i") 'z-toggle-activate-mark)

(require 'goto-chg)
(global-set-key (kbd "C-.") 'goto-last-change)
(global-set-key (kbd "C-,") 'goto-last-change-reverse)

(require 'register-channel)
(register-channel-mode)
(set-register ?5 '(file . "~/Projects/NOTES.org"))
(define-key register-channel-mode-map (kbd "M-g 5")
  'register-channel-describe-register)

(defun all-frames-to-messages-buffer ()
  "make all frames display the *Messages* buffer only after
storing current frame configuration to register 8."
  (interactive)
  (frame-configuration-to-register ?8)
  (dolist (f (frame-list))
    (let ((w (frame-first-window f)))
      (delete-other-windows w)
      (set-window-buffer w "*Messages*"))))
(define-key register-channel-mode-map (kbd "M-g 8")
  'all-frames-to-messages-buffer)

(require 'avy)
(setq avy-style 'at-full)
(eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "M-j") 'avy-isearch))
(global-set-key (kbd "M-j") 'avy-goto-word-1)
(defun z-goto-char (char &optional arg)
  "Call avy-goto-char unless char is RET or SPC, when we call
avy-goto-line or avy-goto-word-1 respectively."
  (interactive (list (read-char "Goto: ")
                     current-prefix-arg))
  (cond ((= char 13) (call-interactively 'avy-goto-line))
        ((= char ?\ ) (call-interactively 'avy-goto-word-1))
        ('t (avy-goto-char char arg))))

(global-set-key (kbd "C-j") 'z-goto-char)
(global-set-key (kbd "C-;") 'avy-goto-line)
(global-set-key (kbd "M-g j") 'avy-goto-char)
(global-set-key (kbd "M-g M-j") 'avy-goto-char)
(global-set-key (kbd "M-g k") 'avy-goto-word-1)
(global-set-key (kbd "M-g M-k") 'avy-goto-word-1)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g M-l") 'avy-goto-line)
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?w ?e ?r ?u ?i ?o ?p ?x ?c ?v ?n ?m))

(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-scope 'frame)
(setq aw-background nil)
(setq aw-keys '(?s ?d ?f ?j ?i ?o ?g ?h ?a ?k ?l ?\;))

(require 'misc)
(defun z-zap-up-to-char (arg char)
  "Similar to zap-up-to-char, but works with multiple cursors."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char "Zap up to char: " t)))
  (zap-up-to-char arg char))
(global-set-key (kbd "M-z") 'z-zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)

(require 'expand-region)
(global-set-key (kbd "C-\\") 'er/expand-region)

(require 'multiple-cursors)
(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m ,") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "M-m M-m") 'mc/mark-all-dwim)
(global-set-key (kbd "M-m m") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "M-m /") 'mc/edit-lines)
(global-set-key (kbd "M-m n") 'mc/insert-numbers)
(global-set-key (kbd "M-m .") 'mc/mark-pop)
(add-to-list 'mc/unsupported-minor-modes 'god-local-mode)

(require 'yasnippet)
(global-set-key (kbd "M-?") 'yas-insert-snippet)

(require 'guide-key)
(setq guide-key/guide-key-sequence
      '("C-x r" "C-x 4" "C-x 8" "C-x t" "C-x C-k" "M-s"))
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode 1)

;; --------------------------------------------------
(require 'god-mode)
(setq god-exempt-major-modes nil)
;; (add-to-list 'god-exempt-major-modes 'ibuffer-mode)
(setq god-exempt-predicates nil)
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

(dolist (i '("c" "g" "j" "k" "l" "n" "p"))
  ;; bind M-g C-i to M-g i
  (global-set-key (kbd (concat "M-g C-" i)) (key-binding (kbd (concat "M-g " i)))))

(defun z-god-mode-update ()
  (cond (god-local-mode (set-face-background 'mode-line "blue4"))
        (t  (set-face-background 'mode-line "#2B2B2B"))))
(add-hook 'god-mode-enabled-hook 'z-god-mode-update)
(add-hook 'god-mode-disabled-hook 'z-god-mode-update)
