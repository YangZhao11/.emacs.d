(use-package grab-region :diminish " ⊛"
  :functions grab-region-move
  :bind ("M-*" . grab-region-mode)
  :config
  (grab-region-remap z-goto-char))


  (defun easy-kill-grab-region ()
    (interactive)
    (easy-kill-mark-region)
    (grab-region-mode))
  (put #'easy-kill-grab-region 'easy-kill-exit t)


(use-package jump-char :ensure
  :bind (("M-." . jump-char-forward)
         ("M-," . jump-char-backward))
  :init
  (setq jump-char-forward-key "M-."
        jump-char-backward-key "M-,")
  :config
  (bind-keys :map jump-char-isearch-map
             ("C-j"      . jump-char-switch-to-ace)
             ("<return>" . jump-char-exit)
             ("RET"      . jump-char-exit))
  (defalias 'ace-jump-char-mode #'avy-goto-char))


(use-package indent-guide
  :diminish indent-guide-mode
  :bind ("C-x t i" . indent-guide-mode)
  :config
  (setq indent-guide-char "│"))


;; Remove ## as begining of comment. Google R style guide insists we use
;; single #.
(defun z-remove-fancy-comments ()
  (interactive)
  (when (eq major-mode 'ess-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\( *\\)## " nil t)
        (replace-match "\\1# " nil nil)))))

(use-package which-key :ensure :diminish which-key-mode
  :bind ("C-x t /" . which-key-mode)
  :config (setq which-key-idle-delay 2)
  (setq which-key-key-replacement-alist
        (append '(("TAB" . "↹") ("DEL" . "⇤")("RET" . "⏎")("SPC" . "␣"))
                which-key-key-replacement-alist)))

(use-package iy-go-to-char :ensure
  :bind (("M-." . iy-go-up-to-char)
         ("M-," . iy-go-to-char-backward))
  :config
  (setq iy-go-to-char-use-key-forward nil
        iy-go-to-char-use-key-backward nil)
  (defun zy-goto-char-continue (n)
    (interactive "p")
    (iy-go-to-or-up-to-continue
     (* iy-go-to-char-start-dir n) 'exclude))
  (defun zy-goto-char-continue-backward (n)
    (interactive "p")
    (iy-go-to-or-up-to-continue
     (- (* iy-go-to-char-start-dir n)) 'include))
  (bind-keys :map iy-go-to-char-keymap
             ("M-." . zy-goto-char-continue)
             ("M-," . zy-goto-char-continue-backward)))

(use-package expand-region :ensure
  :bind ("C-\\" . er/expand-region)
  :config
  (setq er/try-expand-list
        (append er/try-expand-list '(mark-paragraph mark-page))))

(use-package change-inner :ensure
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package multiple-cursors :ensure
  :init
  (global-unset-key (kbd "M-m"))
  :bind (("M-m ," . mc/mark-more-like-this-extended)
         ("M-m m" . mc/mark-all-dwim)
         ("M-m M-m" . mc/mark-all-like-this-dwim)
         ("M-m /" . mc/edit-lines)
         ("M-m n" . mc/insert-numbers)
         ("M-m ." . mc/mark-pop))
  :config
  (add-to-list 'mc/unsupported-minor-modes 'god-local-mode)
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))


;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)
(defun z-cperl-mode-hook ()
  (define-key cperl-mode-map (kbd "C-h f") 'cperl-perldoc)
  (define-key cperl-mode-map "\C-m" 'newline-and-indent)
  (setq cperl-invalid-face nil
        cperl-electric-parens nil
        cperl-electric-keywords nil
        cperl-indent-level 4))
(add-hook 'cperl-mode-hook 'z-cperl-mode-hook)

(defun z-LaTeX-mode-hook ()
  (define-key LaTeX-mode-map (kbd "C-x `") 'next-error)
  (define-key LaTeX-mode-map (kbd "C-x <f1>") 'TeX-ispell-document)
  (turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        TeX-auto-save t
        TeX-parse-self t))
(add-hook 'LaTeX-mode-hook 'z-LaTeX-mode-hook)
;;'(TeX-PDF-mode t)
;; '(TeX-output-view-style (quote (("^dvi$" "." "evince %dS %d") ("^pdf$" "." "evince %o") ("^html?$" "." "sensible-browser %o"))))
;; '(TeX-view-style (quote (("." "%(o?)evince %dS %d"))))

;; (require 'jump-char)
;; (setq jump-char-forward-key "."
;;       jump-char-backward-key ",")
;; (define-key jump-char-isearch-map (kbd "C-j") 'jump-char-switch-to-ace)
;; (defalias 'ace-jump-char-mode 'avy-goto-char)
;; (global-set-key (kbd "M-m") 'jump-char-forward)
;; (global-set-key (kbd "M-M") 'jump-char-backward)

;; replaced with ace-window
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

;; Set frame font to "Monospace-nn", where nn is the key pressed last
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

(defun toggle-variable (var)
  "toggles boolean variable"
  (interactive "vToggle variable: ")
  (set var (not (symbol-value var)))
  (message "%s set to %s" var (symbol-value var)))

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
(global-set-key (kbd "C-x <f8>") 'z-maybe-gud)

(defun z-maybe-recompile (&optional arg)
  "recompile if possible"
  (interactive "P")
  (if (and (fboundp 'recompile) (not arg))
      (recompile)
    (call-interactively 'compile)))
(global-set-key [f5] 'z-maybe-recompile)

(require 'hydra)

(defhydra hydra-mark (global-map "M-m" :idle 1.0)
  "mark"
  ("s" mark-sexp "sexp")
  ("f" mark-defun "defun")
  ("w" mark-word "word")
  ("e" mark-end-of-sentence "sentence")
  ("h" mark-paragraph "paragraph")
  ("p" mark-page "page")
  ("\\" er/expand-region "expand")
  ("|" er/contract-region "contract")
  ("u" backward-up-list "up list")
  ("d" down-list "down list")
  ("b" backward-sexp "backward list")
  ("f" forward-sexp "forward list")
  ("x" z-exchange-point-and-mark "exchange")
  ("i" z-toggle-activate-mark "activate")
  ("q" nil "cancel")
  )
(global-set-key (kbd "M-m SPC") 'hydra-mark/body)

;; (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
;;                            :color pink
;;                            :hint nil
;;                            :post (deactivate-mark))
;;   "
;;   ^_p_^     _d_elete    s_t_ring  e_x_change
;; _b_   _f_   _k_ill      _y_ank    _/_undo
;;   ^_n_^     ne_w_-copy  _r_eset   _q_uit
;; "
;;   ("b" backward-char)
;;   ("f" forward-char)
;;   ("p" previous-line)
;;   ("n" next-line)
;;   ("x" z-exchange-point-and-mark)
;;   ("w" copy-rectangle-as-kill)
;;   ("d" delete-rectangle)
;;   ("r" (if (region-active-p)
;;            (deactivate-mark)
;;          (rectangle-mark-mode 1)))
;;   ("y" yank-rectangle)
;;   ("/" undo)
;;   ("t" string-rectangle)
;;   ("k" kill-rectangle)
;;   ("q" nil))
;; (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)
