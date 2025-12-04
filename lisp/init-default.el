;; -*- lexical-binding: t -*-

;; Defaults
(setq-default indent-tabs-mode nil
              indicate-empty-lines 't
              frame-title-format "Emacs"
              ispell-program-name "aspell"
              page-delimiter "\\(^\f\\|-\\{20,\\}$\\)")
(setq inhibit-startup-screen t
      visible-bell 't
      set-mark-command-repeat-pop 't
      sentence-end-double-space nil
      require-final-newline 't
      tramp-default-method "ssh"
      text-scale-mode-step 1.1
      scroll-margin 2
      shift-select-mode nil
      mouse-yank-at-point 't
      delete-by-moving-to-trash 't
      default-input-method 'TeX)

(defun set-frame-menubar (&optional frame)
  "Enable / disable the menubar in FRAME (default: selected
    frame). Enable only on a graphical display on mac."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                       (if (and (display-graphic-p frame)
                                (eq system-type 'darwin))
                           1 0)))
(add-hook 'after-make-frame-functions 'set-frame-menubar)

(mouse-wheel-mode 1)
(blink-cursor-mode 1)
(transient-mark-mode 1)
(global-font-lock-mode 't)
(temp-buffer-resize-mode 1)

(setq use-short-answers t)

(show-paren-mode 1)
(setq show-paren-when-point-in-periphery 't)
;; overlay face is hard coded and does not work well on terminal
(setq show-paren-context-when-offscreen 't)
(setq blink-matching-paren-highlight-offscreen 't)
(electric-pair-mode 1)

(savehist-mode 1)

(defun first-arg (first &rest _)
  "Return first arg, ignore the rest."
  first)

(defun z-buffer-status (&optional prop)
  "1-char status symbol for current buffer.

PROP set to true to add properties for mode line."
  (let ((p (if prop #'propertize #'first-arg)))
    (cond
     ((derived-mode-p 'comint-mode 'term-mode 'vterm-mode)
      (funcall p
       "∞"                              ; 󰆍
       'help-echo "Interactive shell"))

     (buffer-read-only
      (funcall p
       "∅"                              ;  󰷢 
       'help-echo 'mode-line-read-only-help-echo
       'local-map (purecopy (make-mode-line-mouse-map
                             'mouse-1
                             #'mode-line-toggle-read-only))
       'mouse-face 'mode-line-highlight))

     ((buffer-modified-p)
      (funcall p
       "♦"                              ;   
       'help-echo 'mode-line-modified-help-echo
       'local-map (purecopy (make-mode-line-mouse-map
                             'mouse-1 #'mode-line-toggle-modified))
       'mouse-face 'mode-line-highlight))
     (:else
      (funcall p
       "♢"                              ;  
       'help-echo "Buffer is not modified")))))

(when (< emacs-major-version 31)
  ;; Add prompt indicator to `completing-read-multiple'. Also see
  ;; `crm-separator'.
  (defun crm-indicator (args)
    (cons (concat "[,] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))
;; only in 31
(setq crm-prompt "[%s] %p")

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(put 'scroll-up-command 'command-semantic 'scroll-up-command)
(put 'scroll-down-command 'command-semantic 'scroll-down-command)

(put 'next-line 'command-semantic 'next-line)
(put 'previous-line 'command-semantic 'previous-line)

(put 'quit-window 'command-semantic 'quit-window)
(put 'self-insert-command 'command-semantic 'self-insert-command)

;; --------------------------------------------------
(load-theme 'doom-zenburn 't)     ; load this first for many defaults.

;; autoload not working yet, load the whole thing
(require 'keymap-hint)

(provide 'init-default)
