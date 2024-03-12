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
(electric-pair-mode 1)

(savehist-mode 1)

;; Add prompt indicator to `completing-read-multiple'. Also see
;; `crm-separator'.
(defun crm-indicator (args)
  (cons (concat "[,] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq read-extended-command-predicate
       #'command-completion-default-include-p)

;; --------------------------------------------------
(load-theme 'doom-zenburn 't)   ; load this first for many defaults.

(provide 'init-default)
