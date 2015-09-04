(use-package browse-kill-ring :ensure
  :bind ("C-M-y" . browse-kill-ring))

(use-package easy-kill :ensure
  :bind ([remap kill-ring-save] . easy-kill)
  :config
  (add-to-list 'easy-kill-alist '(?p paragraph "\n"))
  (setq easy-kill-unhighlight-key " ")
  (bind-keys
   :map easy-kill-base-map
   ("k" . easy-kill-region)
   ("m" . easy-kill-mark-region)
   ([remap z-exchange-point-and-mark] . easy-kill-exchange-point-and-mark)))

(defun cycle-spacing-0 ()
    (interactive) (cycle-spacing 0))
(bind-keys ("M-SPC" . cycle-spacing)
           ("M-\\"  . cycle-spacing-0))

(global-set-key (kbd "M-s M-o") 'multi-occur-in-matching-buffers)
;; occur-edit-mode in occur mode key binding is 'e'
(global-set-key (kbd "M-s g") 'grep)
(global-set-key (kbd "M-s M-g") 'rgrep)

;; Decouple exchange-point-and-mark and activating region.
(defun z-exchange-point-and-mark (&optional arg)
  "Like exchange-point-and-mark, but arg means toggle active
region, instead of inactivate region."
  (interactive "P")
  (let ((active (or (and arg (not (use-region-p)))
                    (and (not arg) (use-region-p)))))
    (if (and (bound-and-true-p rectangle-mark-mode)
             (fboundp 'rectangle-exchange-point-and-mark))
        (rectangle-exchange-point-and-mark (not active))
      (exchange-point-and-mark (not active)))))
(global-set-key (kbd "C-x C-x") 'z-exchange-point-and-mark)

(defun z-toggle-activate-mark () (interactive)
  (if (region-active-p)
      (deactivate-mark)
      (activate-mark)))
(global-set-key (kbd "M-=") 'z-toggle-activate-mark)

(use-package dired-x
  :bind ("C-x C-j" . dired-jump)
  :init
  (setq dired-x-hands-off-my-keys nil))

(defun ediff-this-buffer ()
  (interactive)
  (if (buffer-modified-p)
      (ediff-current-file)
    (ediff-backup (buffer-file-name))))
(global-set-key (kbd "C-x C-d") 'ediff-this-buffer)

(defun shrink-other-window-if-larger-than-buffer ()
    (interactive)
    "Shrink other window if larger than buffer"
    (shrink-window-if-larger-than-buffer
     (next-window (selected-window) nil nil)))
(global-set-key (kbd "C-x _") 'shrink-other-window-if-larger-than-buffer)
(global-set-key (kbd "C-x 9") 'delete-other-windows-vertically)

;; F1 for help.
(global-set-key [f2] 'eshell)
;; F3 and F4 for macros
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

(defun isearch-exit-other-end ()
    "Exit isearch, but at the other end of the search string.
  This is useful when followed by an immediate kill."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))
(define-key isearch-mode-map (kbd "M-RET") 'isearch-exit-other-end)

;(setq-default show-trailing-whitespace t)
(defun toggle-show-trailing-whitespace ()
   "Toggle show-trailing-whitespace"
   (interactive)
   (setq show-trailing-whitespace (not show-trailing-whitespace))
   (message "show-trailing-whitespace set to %s" show-trailing-whitespace))

;; Toggle commands
(global-set-key (kbd "C-x t a") 'abbrev-mode)
(diminish 'abbrev-mode " ∂A")
(add-hook 'text-mode-hook 'abbrev-mode)

(global-set-key (kbd "C-x t c") 'highlight-changes-mode)
(global-set-key (kbd "C-x t d") 'eldoc-mode)
(bind-key "C-x t f" 'auto-fill-mode)
(diminish 'auto-fill-function " ¶")
(add-hook 'text-mode-hook 'auto-fill-mode)

(global-set-key (kbd "C-x t h") 'hi-lock-mode)

(use-package which-key :ensure :diminish which-key-mode
  :bind ("C-x t k" . which-key-mode)
  :config (setq which-key-idle-delay 2))

(use-package flyspell
  :bind (("C-x t l" . flyspell-mode)
         ("C-x t ;" . flyspell-prog-mode))
  :config
  (setcdr (assq 'flyspell-mode minor-mode-alist)
          `((" " (:propertize "α"
                              face flyspell-incorrect
                              mouse-face mode-line-highlight
                              local-map ,mode-line-minor-mode-keymap
                              help-echo "Fly Spell")))))
(add-hook 'text-mode-hook 'flyspell-mode)

(global-set-key (kbd "C-x t n") 'linum-mode)
(global-set-key (kbd "C-x t o") 'outline-minor-mode)

(use-package rainbow-delimiters :ensure
  :bind ("C-x t p" . rainbow-delimiters-mode)
  :init  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(global-set-key (kbd "C-x t r") 'hs-minor-mode)
(use-package whitespace :diminish " ␣"
  :bind ("C-x t s" . whitespace-mode))
(global-set-key (kbd "C-x t t") 'toggle-show-trailing-whitespace)
(global-set-key (kbd "C-x t v") 'view-mode)
(global-set-key (kbd "C-x t w") 'subword-mode)
(global-set-key (kbd "C-x t W") 'superword-mode)
(global-set-key (kbd "C-x t SPC") 'global-hl-line-mode)

(setq ctl-j-map (make-sparse-keymap))
(use-package goto-chg :ensure
  :bind (("M-i" . goto-last-change)
         ("M-o" . goto-last-change-reverse)))

(use-package register-channel :ensure
  :config
  (register-channel-mode)
  (set-register ?5 '(file . "~/Projects/NOTES.org"))
  (define-key register-channel-mode-map (kbd "M-g 5")
    'register-channel-describe-register))

(defun all-frames-to-messages-buffer ()
  "make all frames display the *Messages* buffer only after
storing current frame configuration to register 8."
  (interactive)
  (frameset-to-register ?8)
  (dolist (f (frame-list))
    (let ((w (frame-first-window f)))
      (delete-other-windows w)
      (set-window-buffer w "*Messages*"))))
(define-key register-channel-mode-map (kbd "M-g 8")
  'all-frames-to-messages-buffer)

(use-package avy :ensure
  :bind* ("C-j" . z-goto-char)
  :bind (:map ctl-j-map ("SPC" . avy-goto-line))
  :config
  (setq avy-style 'at-full
        avy-keys
        '(?s ?d ?f ?g ?h ?j ?k ?l ?w ?e ?r ?u ?i ?o ?c ?v ?b ?n ?m))
  (eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "C-j") 'avy-isearch))

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

;; --------------------------------------------------
(defvar z-real-mode-line-bg (face-background 'mode-line))
(use-package god-mode :ensure
  :bind ("ESC ESC" . god-mode-all)
  :config
  (defvar z-god-mode-lighter "")
  (setcdr (assq 'god-local-mode minor-mode-alist)
          '((t z-god-mode-lighter)))
  (defvar z-god-state 'normal)
  (defvar z-god-mode-color nil)
  (defvar z-real-mode-line-bg nil)
  (setq z-god-states
    '((normal " ⌘" "#153E7E" (nil . "C-") ("g" . "M-") ("h" . "C-M-"))
      (cm " ⌘⌥" "#7D053F" (nil . "C-M-") ("g" . "C-"))
      (meta " ⌥" "#055D30" (nil . "M-"))))
  (defun z-god-mode-update ()
    (unless z-real-mode-line-bg
      (setq z-real-mode-line-bg (face-background 'mode-line)))
    (cond (god-local-mode
           (set-face-background 'mode-line z-god-mode-color))
          (t  (set-face-background 'mode-line z-real-mode-line-bg))))
  (defun z-god-set-state (state)
    (let ((s (cdr (assq state z-god-states))))
      (setq z-god-mode-lighter (car s)
            z-god-mode-color (cadr s)
            god-mod-alist (cddr s)
            z-god-state state)
      (z-god-mode-update)))

  (defun z-god-mode-toggle-meta ()
    (interactive)
    (z-god-set-state (if (eq z-god-state 'meta) 'normal 'meta)))
  (defun z-god-mode-toggle-cm ()
    (interactive)
    (z-god-set-state (if (eq z-god-state 'cm) 'normal 'cm)))

  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  (bind-keys :map god-local-mode-map
             ("z" . repeat)
             ("i" . god-mode-all)
             ("[" . z-god-mode-toggle-cm)
             ("]" . z-god-mode-toggle-meta)
             ("#" . server-edit))

  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "ESC ESC") 'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "ESC ESC") 'god-mode-isearch-disable)

  ;; bind symbols to M-?
  (dolist (i '("!" "@" "$" "%" "^" "&" "*" "{" "}"
               "<" ">" ";" ":" "|" "="))
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

  (add-hook 'god-mode-enabled-hook (lambda () (z-god-set-state 'normal)))
  (add-hook 'god-mode-disabled-hook 'z-god-mode-update))
