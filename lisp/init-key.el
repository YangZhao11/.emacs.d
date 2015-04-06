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

(global-set-key [f1] 'ispell-word)
(global-set-key (kbd "C-x <f1>") 'ispell-comments-and-strings)
(global-set-key [f2] 'eshell)
;; F3 and F4 for macros
(global-set-key [f5] 'z-maybe-recompile)
;(global-set-key (kbd "C-x <f5>") 'rgrep)
;;(global-set-key [f6] 'z-maybe-gud)
(global-set-key [f7] 'gud-up)
(global-set-key [(shift f7)] 'gud-down)
(global-set-key [f8] 'gud-next)
(global-set-key [(shift f8)] 'gud-step)
(global-set-key (kbd "C-x <f8>") 'z-maybe-gud)
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

(global-set-key (kbd "C-x 9") 'delete-other-windows-vertically)
(global-set-key (kbd "M-9") (lambda () (interactive)
                              (other-window -1)))
(global-set-key (kbd "M-0") 'other-window)
(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(global-set-key (kbd "M-(") (lambda () (interactive)
                              (transpose-windows -1)))
(global-set-key (kbd "M-)") 'transpose-windows)

;; set frame font to "Monospace-nn", where nn is the key pressed last
;; with a lookup in font-size-by-digit. That is, if invoked by "C-x
;; C-1" frame font will be set to "Monospace-8".
(setq font-size-by-digit [30 8 9 10 11 12 14 16 19 24])
(setq frame-font-name-format "Liberation Mono-%d")
(defun set-frame-font-size-self-digit (&optional arg)
  (interactive "p")
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0))
         (font-size (if (and (> arg 5) (<= arg 30)) arg
                      (elt font-size-by-digit digit)))
         (font-name (format frame-font-name-format font-size)))
    (set-frame-font font-name t)
    (message "Frame font set to %s" font-name)))

;; This is not used now
(defun toggle-variable (var)
  "toggles boolean variable"
  (interactive "vToggle variable: ")
  (set var (not (symbol-value var)))
  (message "%s set to %s" var (symbol-value var)))

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
storing current frame configuration to register 8."
  (interactive)
  (frame-configuration-to-register ?8)
  (dolist (f (frame-list))
    (let ((w (frame-first-window f)))
      (delete-other-windows w)
      (set-window-buffer w "*Messages*"))))
(define-key register-channel-mode-map (kbd "M-g 8") 'all-frames-to-messages-buffer)

(require 'ace-jump-mode)
(global-set-key (kbd "M-g M-g") 'ace-jump-mode)
(global-set-key (kbd "M-g g") 'ace-jump-mode)
(global-set-key (kbd "M-g M-h") 'ace-jump-line-mode)
(global-set-key (kbd "M-g h") 'ace-jump-line-mode)
(global-set-key (kbd "M-g M-f") 'ace-jump-char-mode)
(global-set-key (kbd "M-g f") 'ace-jump-char-mode)
(global-set-key (kbd "M-g l") 'goto-line)
(global-set-key (kbd "M-g M-l") 'goto-line)

(defun z-maybe-recompile (&optional arg)
  "recompile if possible"
  (interactive "P")
  (if (and (fboundp 'recompile) (not arg))
      (recompile)
    (call-interactively 'compile)))

(setq preferred-debugger-alist
      '((c-mode . gdb)
        (c++-mode . gdb)
        (cperl-mode . perldb)
        (python-mode . pdb)
        (jde-mode . jdb)))
(defun z-maybe-gud ()
  "Run gdb if not already running, otherwise bring it to front"
  (interactive)
  (require 'gud)
  (if (and (boundp 'gud-comint-buffer)  ;find running gdb process
           gud-comint-buffer
           (buffer-name gud-comint-buffer)
           (get-buffer-process gud-comint-buffer))
      (if (fboundp 'gdb-restore-windows)
           (gdb-restore-windows)
        (pop-to-buffer gud-comint-buffer))
    (call-interactively
     (or (cdr (assq major-mode preferred-debugger-alist))
         'gdb))))

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

(dolist (i '("1" "2" "3" "4" "5" "6" "7" "8"))
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
