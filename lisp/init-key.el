; -*- coding: utf-8 -*-

;;; Code:
(use-package browse-kill-ring :ensure
  :bind ("C-M-y" . browse-kill-ring))

(use-package anchored-transpose :ensure
  :commands anchored-transpose
  :preface
  (defun z-transpose (arg)
    (interactive "*P")
    (if (or arg (use-region-p))
        (call-interactively #'anchored-transpose)
      (call-interactively #'transpose-chars)))
  :bind ("C-t" . z-transpose))


(use-package grab-region :diminish " ⊕"
  :functions grab-region-move
  :bind ("M-+" . grab-region-mode)
  :config
  (grab-region-remap z-goto-char))


(use-package easy-kill :ensure
  :functions easy-kill-mark-region
  :bind ([remap kill-ring-save] . easy-kill)
  :config

  (defun easy-kill-transpose ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'anchored-transpose)))
  (defun easy-kill-wrap-region ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'self-insert-command)))
  (defun easy-kill-grab-region ()
    (interactive)
    (easy-kill-mark-region)
    (grab-region-mode))
  (defun easy-kill-indent-region ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'indent-region)))

  (add-to-list 'easy-kill-alist '(?p paragraph "\n"))
  (setq easy-kill-unhighlight-key " ")

  (defun easy-kill-transpose ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'anchored-transpose)))
  (defun easy-kill-wrap-region ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'self-insert-command)))
  (defun easy-kill-indent-region ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'indent-region)))
  (put #'easy-kill-transpose 'easy-kill-exit t)
  (put #'easy-kill-wrap-region 'easy-kill-exit t)
  (put #'easy-kill-indent-region 'easy-kill-exit t)
  (put #'easy-kill-grab-region 'easy-kill-exit t)

  (bind-keys
   :map easy-kill-base-map
   ("k"  . easy-kill-region)
   ("g"  . easy-kill-grab-region)
   ("+"  . easy-kill-grab-region)
   ("m"  . easy-kill-mark-region)
   ("t"  . easy-kill-transpose)
   ("("  . easy-kill-wrap-region)
   (")"  . easy-kill-wrap-region)
   ("["  . easy-kill-wrap-region)
   ("]"  . easy-kill-wrap-region)
   ("{"  . easy-kill-wrap-region)
   ("}"  . easy-kill-wrap-region)
   ("\"" . easy-kill-wrap-region)
   ("'"  . easy-kill-wrap-region)
   ("\\" . easy-kill-indent-region)))

(defun cycle-spacing-0 ()
  "Remove adjacent spaces, but undo if the command is issued the second time."
  (interactive) (cycle-spacing 0))
(bind-keys ("M-SPC" . cycle-spacing)
           ("M-\\"  . cycle-spacing-0))

(defun toggle-selective-display (column)
  "Toggle selective display, defaulting to current column"
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))
(bind-key "C-x $" 'toggle-selective-display)

(defun isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string. This is useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))
(bind-keys :map isearch-mode-map
              ("M-RET" . isearch-exit-other-end))
(bind-keys ("M-s M-o" . multi-occur-in-matching-buffers)
           ("M-s g"   . grep)
           ("M-s M-g" . rgrep))

;; Decouple exchange-point-and-mark and activating region.
(defun z-exchange-point-and-mark (&optional arg)
  "Like `exchange-point-and-mark', but ARG means toggle active region, instead of inactivate region."
  (interactive "P")
  (let ((active (or (and arg (not (use-region-p)))
                    (and (not arg) (use-region-p)))))
    (if (and (bound-and-true-p rectangle-mark-mode)
             (fboundp 'rectangle-exchange-point-and-mark))
        (rectangle-exchange-point-and-mark (not active))
      (exchange-point-and-mark (not active)))))
(bind-key [remap exchange-point-and-mark] #'z-exchange-point-and-mark)

(defun z-toggle-activate-mark ()
  "Toggle active region, without moving the mark."
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (activate-mark)))
(bind-key "M-=" #'z-toggle-activate-mark)

(use-package imenu
  :bind ("C-x j" . imenu))

(use-package dired-x
  :bind ("C-x C-j" . dired-jump)
  :init
  (setq dired-x-hands-off-my-keys nil))

(use-package find-file
  :bind ("C-x C-r" . ff-find-other-file))

(defun ediff-this-buffer ()
  "Call ediff on this buffer, with the version on disk or backup."
  (interactive)
  (if (buffer-modified-p)
      (ediff-current-file)
    (ediff-backup (buffer-file-name))))
(bind-key "C-x C-d" #'ediff-this-buffer)

(defun shrink-other-window-if-larger-than-buffer ()
    "Shrink other window if larger than buffer."
    (interactive)
    (shrink-window-if-larger-than-buffer
     (next-window (selected-window) nil nil)))
(bind-keys ("C-x _" . shrink-other-window-if-larger-than-buffer)
           ("C-x 9" . delete-other-windows-vertically))

;; F1 for help.
(bind-key "<f2>" #'eshell)
;; F3 and F4 for macros
;; F5 and F6 bound for org-mode stuff.
(use-package gud
  :bind (("<f7>"   . gud-up)
         ("S-<f7>" . gud-down)
         ("<f8>"   . gud-next)
         ("S-<f8>" . gud-step)
         ("<f9>"   . gud-finish)))

(defun toggle-one-window ()
  "Change to one window (C-x 1) if applicable, otherwise show other buffer in other window."
  (interactive)
  (if (window-parent)
      (delete-other-windows)
    (display-buffer (other-buffer) t)))
(bind-keys ("<f10>" . toggle-one-window)
           ("<f11>" . shrink-window)
           ("<f12>" . enlarge-window))

(defun z-prev-buffer-next-window ()
  "Switch other window to previous buffer."
  (interactive)
  (switch-to-prev-buffer (next-window)))
(defun z-next-buffer-next-window ()
  "Switch other window to next buffer."
  (interactive)
  (switch-to-next-buffer (next-window)))
(bind-keys ("M-9" . switch-to-prev-buffer)
           ("M-0" . switch-to-next-buffer)
           ("M-(" . z-prev-buffer-next-window)
           ("M-)" . z-next-buffer-next-window))

;(setq-default show-trailing-whitespace t)
(defun toggle-show-trailing-whitespace ()
   "Toggle `show-trailing-whitespace'."
   (interactive)
   (setq show-trailing-whitespace (not show-trailing-whitespace))
   (message "show-trailing-whitespace set to %s" show-trailing-whitespace))

;; Toggle commands
(global-set-key (kbd "C-x t a") #'abbrev-mode)
(diminish 'abbrev-mode " ∂A")

(use-package beacon :ensure :diminish beacon-mode
  :bind ("C-x t b" . beacon-mode)
  :config
  (add-hook 'beacon-dont-blink-predicates
            (lambda () (not (display-graphic-p))))
  (setq beacon-dont-blink-major-modes
        '(inferior-ess-mode
          magit-status-mode magit-popup-mode
          gnus-summary-mode gnus-group-mode)))
(beacon-mode 1)

(global-set-key (kbd "C-x t c") #'highlight-changes-mode)
(global-set-key (kbd "C-x t d") #'which-function-mode)
(bind-key "C-x t f" #'auto-fill-mode)
(diminish 'auto-fill-function " ¶")

(use-package flyspell :diminish " ⍹"
  :bind (("C-x t l" . flyspell-mode)
         ("C-x t ;" . flyspell-prog-mode)))

(use-package rainbow-delimiters :ensure
  :bind ("C-x t p" . rainbow-delimiters-mode)
  :init  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package autorevert
  :diminish (auto-revert-mode . " ↻")
  :bind ("C-x t r" . auto-revert-mode))

(use-package whitespace :diminish " ␣"
  :bind ("C-x t s" . whitespace-mode))

(bind-keys ("C-x t e"   . hs-minor-mode)
           ("C-x t h"   . hi-lock-mode)
           ("C-x t n"   . linum-mode)
           ("C-x t o"   . outline-minor-mode)
           ("C-x t t"   . toggle-show-trailing-whitespace)
           ("C-x t v"   . view-mode)
           ("C-x t W"   . superword-mode)
           ("C-x t w"   . subword-mode)
           ("C-x t SPC" . hl-line-mode))

(use-package register-channel :ensure
  :config
  (register-channel-mode)
  (set-register ?5 '(file . "~/Projects/NOTES.org"))
  (bind-key "M-g 5" #'register-channel-describe-register
            register-channel-mode-map))

(defun all-frames-to-messages-buffer ()
  "Make all frames display the *Messages* buffer, only after storing current frame configuration to register 8."
  (interactive)
  (frameset-to-register ?8)
  (dolist (f (frame-list))
    (let ((w (frame-first-window f)))
      (delete-other-windows w)
      (set-window-buffer w "*Messages*"))))
(bind-key "M-g 8" #'all-frames-to-messages-buffer register-channel-mode-map)

(defvar ctl-j-map)
(setq ctl-j-map (make-sparse-keymap))
(use-package goto-chg :ensure
  :bind (("M-i" . goto-last-change)
         ("M-o" . goto-last-change-reverse)))

(use-package avy :ensure
  :bind* ("C-j" . z-goto-char)
  :bind (:map ctl-j-map
              ("SPC" . avy-goto-line)
              ("TAB" . avy-copy-region))
  :config
  (require 'subword)
  (setq avy-styles-alist '((avy-goto-char . de-bruijn))
        avy-keys
        '(?s ?d ?f ?g ?h ?j ?k ?l ?w ?e ?r ?u ?i ?o))
  (eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "C-j") #'avy-isearch))

  (defun z-goto-char (char &optional arg)
  "Call avy-goto-char or avy-goto-subword-1, but respect bindings
in ctl-j-map first."
  (interactive (list (read-char "C-j ")
                     current-prefix-arg))
  (let ((act (lookup-key ctl-j-map (char-to-string char))))
    (cond (act (call-interactively act))
          ((string-match-p "[[:alpha:]]" (char-to-string char))
           (avy-goto-subword-1 char arg))
          ('t (avy-goto-char char arg))))))

(use-package ace-window :ensure
  :bind* ("M-j" . ace-window)
  :config
  (setq aw-scope 'frame
        aw-background nil
        aw-keys '(?s ?d ?f ?j ?i ?o ?g ?h ?a ?k ?l ?\;)))

(use-package misc
  :commands zap-up-to-char
  :bind (("M-z" . zap-up-to-char)
         ("M-Z" . zap-to-char)))

(use-package smex :ensure
  :bind ("M-x" . smex))

(use-package xref
  :if (not (featurep 'google))
  :bind (("C-x C-." . xref-find-definitions)
         ("C-x C-," . xref-pop-marker-stack)))

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

;; --------------------------------------------------
(defvar z-god-mode-lighter "")
(setq-default mode-line-format
      (cons '(:eval z-god-mode-lighter)
            (default-value 'mode-line-format)))

(use-package god-mode :ensure
  :bind ("ESC ESC" . god-mode-all)
  :diminish god-local-mode
  :config

  (defvar z-god-state 'normal)
  (defvar z-god-states
        `((normal (:propertize " ⌘ " face
                   (:background "#4DB0FF" :foreground "black"))
                  (nil . "C-") ("g" . "M-") ("h" . "C-M-"))
          (cm (:propertize "⌥⌘ " face
                   (:background "#FF6088" :foreground "black"))
              (nil . "C-M-") ("g" . "C-"))
          (meta (:propertize " ⌥ " face
                   (:background "#FF38E0" :foreground "black"))
                (nil . "M-"))))

  (defun z-god-set-state (state)
    (let ((s (cdr (assq state z-god-states))))
      (setq z-god-mode-lighter (car s)
            god-mod-alist (cdr s)
            z-god-state state))
    (force-mode-line-update))

  (defun mortal-mode-return ()
    (interactive)
    (unless god-global-mode
      (god-mode-all)))
  (define-minor-mode mortal-mode
    "Allow temporary departures from god-mode."
    :global 't
    :keymap '(([?\C-m] .
               (menu-item "" nil
                          :filter (lambda (&optional _)
                                    (when  (not (minibufferp))
                                      #'mortal-mode-return)))))
    (when (and mortal-mode god-local-mode)
      (if god-global-mode (god-mode-all))
      (setq z-god-mode-lighter
            '(:propertize " ⎀ " face
              (:background "#4DFFA0" :foreground "black")))))

  (defun z-god-mode-toggle-meta ()
    (interactive)
    (z-god-set-state (if (eq z-god-state 'meta) 'normal 'meta)))
  (defun z-god-mode-toggle-cm ()
    (interactive)
    (z-god-set-state (if (eq z-god-state 'cm) 'normal 'cm)))

  (setq god-exempt-major-modes nil
        god-exempt-predicates nil)
  (defalias 'true-self-insert-command 'self-insert-command)
  (bind-keys :map god-local-mode-map
             ("z" . repeat)
             ("i" . mortal-mode)
             ("[" . z-god-mode-toggle-cm)
             ("(" . true-self-insert-command)
             (")" . true-self-insert-command)
             ("`" . next-error)
             ("$" . toggle-selective-display)
             ("#" . server-edit)
             ("*" . calc-dispatch))

  (require 'god-mode-isearch)
  (bind-key "ESC ESC" #'god-mode-isearch-activate isearch-mode-map)
  (bind-key "ESC ESC" #'god-mode-isearch-disable god-mode-isearch-map)

  ;; bind symbols to M-?
  (dolist (i '("!" "@" "%" "^" "&" "{" "}"
               "<" ">" ";" ":" "|" "\\" "+" "=" "?"))
    (define-key god-local-mode-map (kbd i)
      (key-binding (kbd (concat "M-" i)))))

  ;; Bind some second level modifier keys with C- prefix for easier
  ;; god-mode access. Directly bind these to commands, instead of making
  ;; it a keyboard macro so that messages work in god-mode.
  (dolist (bindings
           '(("C-x" "0" "1" "2" "3" "9" "[" "]")
             ("M-g" "1" "2" "3" "4" "5" "6" "7" "8" "c" "n" "p")))
    (let ((prefix (car bindings))
          (chars (cdr bindings)))
      (dolist (i chars)
        (global-set-key (kbd (concat prefix " C-" i))
                        (key-binding (kbd (concat prefix " " i)))))))

  (defun z-god-mode-enabled-hook ()
    (mortal-mode 0)
    (z-god-set-state z-god-state))
  (add-hook 'god-mode-enabled-hook 'z-god-mode-enabled-hook)

  (defun z-god-mode-disabled-hook ()
    (unless mortal-mode
      (setq z-god-mode-lighter nil)
      (setq z-god-state 'normal)))
  (add-hook 'god-mode-disabled-hook 'z-god-mode-disabled-hook))
