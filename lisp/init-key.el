; -*- coding: utf-8 -*-

;;; Code:
(eval-when-compile
  (require 'use-package)
  (require 'hydra))

(eval-after-load "quail/Latin-ltx"
  '(let ((quail-current-package (assoc "TeX" quail-package-alist)))
     (quail-define-rules ((append . t))
                         ("^\\alpha" ?ᵅ)
                         ("\\sqrt" ?√))))

(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) #'kill-region #'backward-kill-word)))

(bind-keys ("<C-M-backspace>" . backward-kill-sexp)
           ("C-x k" . kill-this-buffer)
           ("C-w" . kill-region-or-backward-word))

(use-package anchored-transpose :ensure
  :commands anchored-transpose
  :preface
  (defun z-transpose (arg)
    (interactive "*P")
    ""
    (call-interactively
     (if (or arg (use-region-p))
         #'anchored-transpose #'transpose-chars)))
  :bind ("C-t" . z-transpose))

(use-package easy-kill :ensure
  :functions easy-kill-mark-region
  :bind ([remap kill-ring-save] . easy-kill)
  :config

  (defun easy-kill-transpose ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'anchored-transpose)))
  (put #'easy-kill-transpose 'easy-kill-exit t)

  (defun easy-kill-wrap-region ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'self-insert-command)))
  (put #'easy-kill-wrap-region 'easy-kill-exit t)

  (defun easy-kill-indent-region ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'indent-region)))
  (put #'easy-kill-indent-region 'easy-kill-exit t)

  (defun easy-kill-comment-dwim ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'comment-dwim)))
  (put #'easy-kill-comment-dwim 'easy-kill-exit t)

  (add-to-list 'easy-kill-alist '(?p paragraph "\n"))
  (setq easy-kill-unhighlight-key " ")

  (bind-keys
   :map easy-kill-base-map
   ("k"  . easy-kill-region)
   ("m"  . easy-kill-mark-region)
   ("t"  . easy-kill-transpose)
   (";"  . easy-kill-comment-dwim)
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
  "Remove adjacent spaces, but undo if the command is issued the
second time."
  (interactive) (cycle-spacing 0))

(defun toggle-selective-display (column)
  "Toggle selective display, defaulting to current column."
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(bind-keys ("M-SPC"  . cycle-spacing)
           ("M-\\"   . cycle-spacing-0)
           ("M-c"    . capitalize-dwim)
           ("M-l"    . downcase-dwim)
           ("M-u"    . upcase-dwim)
           ("C-x $"  . toggle-selective-display))

(defun isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string. This is
useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))
(bind-keys :map isearch-mode-map
           ("M-RET" . isearch-exit-other-end)
           ("M-k" . isearch-yank-word-or-char))
(bind-keys ("M-s M-o" . multi-occur-in-matching-buffers)
           ("M-s g"   . grep)
           ("M-s M-g" . rgrep))

;; Decouple exchange-point-and-mark and activating region.
(defun z-exchange-point-and-mark (&optional arg)
  "Like `exchange-point-and-mark', but ARG means toggle active region,
instead of inactivate region."
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

(use-package find-file
  :bind ("C-x C-r" . ff-find-other-file))

(defun ediff-this-buffer ()
  "Call ediff on this buffer, with the version on disk or backup."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not associated with a file"))
  (if (buffer-modified-p)
      (ediff-current-file)
    (ediff-backup (buffer-file-name))))
(bind-keys ("C-x =" . #'ediff-this-buffer)
           ("C-x M-=" . 'what-cursor-position))

(defun shrink-other-window-if-larger-than-buffer ()
    "Shrink other window if larger than buffer."
    (interactive)
    (shrink-window-if-larger-than-buffer
     (next-window (selected-window) nil nil)))
(bind-keys ("C-x _" . shrink-other-window-if-larger-than-buffer)
           ("C-x 9" . delete-other-windows-vertically))

(bind-key "C-z" nil)

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
  "Change to one window (C-x 1) if applicable, otherwise show other
buffer in other window."
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

(defun toggle-show-trailing-whitespace ()
   "Toggle `show-trailing-whitespace'."
   (interactive)
   (setq show-trailing-whitespace (not show-trailing-whitespace))
   (message "show-trailing-whitespace set to %s" show-trailing-whitespace))

;; Toggle commands

(defmacro ballotbox (var &optional pos)
  (if pos
  `(if (bound-and-true-p ,var) ,pos "☐")
  `(if (bound-and-true-p ,var) "☒" "☐")))

(defhydra hydra-toggle (:color blue :hint nil)
  "
Toggle:
%s(ballotbox rainbow-delimiters-mode) rainbow-_d_elimiters  ^^ %s(ballotbox abbrev-mode \"∂\") _a_bbrev         %s(ballotbox outline-minor-mode) _o_utline-minor-mode ^^ %s(ballotbox company-mode \"©\") co_m_pany
%s(ballotbox color-identifiers-mode) color-_i_dentifiers   ^^ %s(ballotbox auto-fill-function \"¶\") auto-_f_ill      %s(ballotbox view-mode) _v_iew-mode          ^^ %s(ballotbox flycheck-mode \"✔\") flychec_k_
%s(ballotbox beacon-mode) _b_eacon              ^^ %s(ballotbox visual-line-mode \"↵\") visual-lin_e_    %s(if (bound-and-true-p subword-mode) \",\" (if (bound-and-true-p superword-mode) \"²\" \"☐\")) sub_w_ord/super_W_ord   %s(ballotbox ycmd-mode \"☯\") _y_cmd
%s(ballotbox hi-lock-mode) _h_i-lock/_c_hanges      %s(ballotbox auto-revert-mode \"↻\") auto-_r_evert    %s(ballotbox flyspell-mode \"⍹\") flyspel_l_/_p_rog       %s(ballotbox which-function-mode) which-f_u_nc
%s(ballotbox whitespace-mode \"␣\") white_s_pace/_t_railing  %s(ballotbox linum-mode) li_n_um
"
  ("a"    abbrev-mode)
  ("b"    beacon-mode)
  ("c"    highlight-changes-mode)
  ("d"    rainbow-delimiters-mode)
  ("e"    visual-line-mode)
  ("f"    auto-fill-mode)
  ("h"    hi-lock-mode)
  ("i"    color-identifiers-mode)
  ("k"    flycheck-mode)
  ("l"    flyspell-mode)
  ("p"    flyspell-prog-mode)
  ("m"    company-mode)
  ("n"    linum-mode)
  ("o"    outline-minor-mode)
  ("r"    auto-revert-mode)
  ("s"    whitespace-mode)
  ("t"    toggle-show-trailing-whitespace)
  ("u"    which-function-mode)
  ("v"    view-mode)
  ("w"    subword-mode)
  ("W"    superword-mode)
  ("y"    ycmd-mode)
  ("SPC"  nil)
)
(bind-key "C-x t" 'hydra-toggle/body)
(diminish 'abbrev-mode " ∂")
(diminish 'auto-fill-function " ¶")
(diminish 'visual-line-mode " ↵")

(use-package beacon :ensure :diminish beacon-mode
  :commands (beacon-mode)
  :init (beacon-mode 1)
  :config
  (add-hook 'beacon-dont-blink-predicates
            (lambda () (not (display-graphic-p))))
  (setq beacon-dont-blink-major-modes
        '(inferior-ess-mode
          magit-status-mode magit-popup-mode
          gnus-summary-mode gnus-group-mode)))

(use-package flyspell :diminish " ⍹"
  :commands (flyspell-mode flyspell-prog-mode))

(use-package rainbow-delimiters :ensure
  :commands (rainbow-delimiters-mode)
  :init  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package autorevert
  :diminish (auto-revert-mode . " ↻")
  :commands (auto-revert-mode))

(use-package whitespace :diminish " ␣"
  :commands (whitespace-mode))

(use-package hideshow :diminish (hs-minor-mode . " ◌")
  :bind ("M-$" . hs-dwim)
  :functions hs-hide-level hs-show-all hs-hide-all hs-toggle-hiding
  :config
  (defun hs-dwim (arg)
    "Hide-show smartly based on ARG:
  Given positive number, call `hs-hide-level'.
  Given negative universal arg, call `hs-hide-all'.
  Given universal arg, call `hs-show-all'.
  Otherwise call `hs-toggle-hiding'."
    (interactive "P")
    (hs-minor-mode 1)
    (cond ((and (numberp arg) (> arg 0)) (hs-hide-level arg))
          ((eq '- arg) (hs-hide-all))
          ((and (listp arg) (numberp (car arg)))
           (hs-show-all) (hs-minor-mode -1))
          ('t (hs-toggle-hiding)))))

(use-package register-channel :ensure
  :config
  (register-channel-mode)
  (set-register ?5 '(file . "~/Projects/notes/NOTES.org"))
  (bind-key "M-g 5" #'register-channel-describe-register
            register-channel-mode-map))

(defun all-frames-to-messages-buffer ()
  "Make all frames display the *Messages* buffer, only after storing
current frame configuration to register 8."
  (interactive)
  (frameset-to-register ?8)
  (dolist (f (frame-list))
    (let ((w (frame-first-window f)))
      (delete-other-windows w)
      (set-window-buffer w "*Messages*"))))
(bind-key "M-g 8" #'all-frames-to-messages-buffer register-channel-mode-map)

(defvar ctl-j-map (make-sparse-keymap)
  "Keymap behind C-j. Called by `z-goto-char'.")

(use-package goto-chg :ensure
  :bind (("M-i" . goto-last-change)
         ("M-I" . goto-last-change-reverse)))

(use-package avy :ensure :defer 5
  :bind ("C-j" . z-goto-char)
  :bind (:map ctl-j-map
              ("SPC" . avy-goto-line))
  :bind (("M-," . avy-backward-char-in-line)
         ("M-." . avy-forward-char-in-line))
  :config
  (require 'subword)
  (setq avy-styles-alist '((avy-goto-char . de-bruijn))
        avy-keys
        '(?s ?d ?f ?g ?h ?j ?k ?l ?w ?e ?r ?u ?i ?o)
        avy-subword-extra-word-chars nil)
  (eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "C-j") #'avy-isearch))

  (defun avy-forward-char-in-line (char)
    "Jump to the currently visible CHAR in the current line."
    (interactive (list (read-char "char: " t)))
    (avy-with avy-goto-char
      (avy--generic-jump
       (regexp-quote (string char))
       avy-all-windows
       avy-style
       (1+ (point))
       (line-end-position))))
  (defun avy-backward-char-in-line (char)
    "Jump to the currently visible CHAR in the current line."
    (interactive (list (read-char "char: " t)))
    (avy-with avy-goto-char
      (avy--generic-jump
       (regexp-quote (string char))
       avy-all-windows
       avy-style
       (line-beginning-position)
       (point))))

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

(use-package ace-window :ensure :defer 6
  :bind* ("M-j" . ace-window)
  :config
  (setq aw-scope 'frame
        aw-background nil
        aw-keys '(?s ?d ?f ?j ?i ?o ?g ?h ?a ?k ?l ?\;)))

(use-package zap-to-char-dwim
  :bind ("M-z" . zap-to-char-dwim))

(use-package subr-x
  :commands (string-trim-right
             string-trim-left))

(use-package imenu-anywhere
  :bind ("M-s M-i" . ivy-imenu-anywhere))

(use-package ivy :diminish ""
  :bind (("M-o" . ivy-switch-buffer)
         ("M-s M-d" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-count-format "")
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers 't)
  (setq ivy-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
          "^\\*LV\\*"
           "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
           "\\[r\\]\\(<[0-9]+>\\)?$" "\\[fundamental\\]\\(<[0-9]+>\\)?$"
           "_region_" " output\\*$" "^TAGS$" "^\*Ido"))

  (defun ivy-insert-action (x)
    (with-ivy-window
      (insert x)))

  (ivy-set-actions t
   '(("s" ivy-insert-action "insert string")))

  (bind-keys :map ivy-minibuffer-map
             ("M-s o" . ivy-occur)
             ("C-j" . ivy-avy)
             ("C-'" . ivy-alt-done)
             ("M-k" . ivy-yank-word)
             ("M-m" . ivy-restrict-to-matches)
             ("ESC ESC" . hydra-ivy/body)))

(use-package counsel :defer 4
  :bind (([remap find-file] . counsel-find-file)
         ("C-x 8 8" . counsel-unicode-char)
         ("C-x b" . counsel-bookmark)
         ("C-x f" . counsel-file-jump)  ; set-fill-column
         ("C-x d" . counsel-dired-jump)
         ("C-x C-d" . dired)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("M-s M-s" . counsel-grep-or-swiper)
         ("M-s i" . counsel-imenu))
  :bind (:map help-map
              ("v" . counsel-describe-variable)
              ("f" . counsel-describe-function)
              ("S" . counsel-info-lookup-symbol))
  :config
  (setq counsel-find-file-ignore-regexp
        "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)\\|\\(\\`\\.\\)"))

(use-package swiper
  :bind (("M-s s" . swiper-all))
  :bind  (:map isearch-mode-map
               ("M-s M-s" . isearch-swiper))
  :config
  (bind-keys :map swiper-map
             ("M-%" . swiper-query-replace)
             ("C-j" . swiper-avy))
  (defun isearch-swiper (regexp)
    "Like isearch-occur, call swiper with current regexp."
    (interactive
     (list (cond
            (isearch-regexp isearch-string)
            (t (regexp-quote isearch-string)))))
    (let ((case-fold-search isearch-case-fold-search)
          ;; Set `search-upper-case' to nil to not call
          ;; `isearch-no-upper-case-p' in `occur-1'.
          (search-upper-case nil)
          (search-spaces-regexp
           (if (if isearch-regexp
                   isearch-regexp-lax-whitespace
                 isearch-lax-whitespace)
               search-whitespace-regexp)))
      (isearch-exit)
      (swiper regexp))))

(use-package xref
  :if (not (featurep 'google))
  :bind (("C-x ." . xref-find-definitions)
         ("C-x ?" . xref-find-references)
         ("C-x ," . xref-pop-marker-stack)))

(defhydra hydra-sexp (:color pink :hint nil)
    "
_u_p    _f_oward  _k_ill  _y_ank
_d_own  _b_ack    _m_ark  _Y_ank-pop
"
    ("SPC" nil)
    ("u" backward-up-list)
    ("d" down-list)
    ("f" forward-sexp)
    ("b" backward-sexp)
    ("k" kill-sexp)
    ("m" mark-sexp)
    ("y" yank)
    ("Y" yank-pop))
(bind-key "M-]" 'hydra-sexp/body)

;; --------------------------------------------------
(defvar z-god-mode-lighter "")
(defvar-local z-god-saved-input-method nil "Saved input method before god-mode")

(defun set-cursor-type (cursor-type)
  (dolist (f (frame-list))
    (modify-frame-parameters f `((cursor-type . ,cursor-type)))))

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
             ("[" . z-god-mode-toggle-cm)
             ("(" . true-self-insert-command)
             (")" . true-self-insert-command)
             ("`" . next-error)
             ("#" . server-edit))

  (require 'god-mode-isearch)
  (bind-key "ESC ESC" #'god-mode-isearch-activate isearch-mode-map)
  (bind-key "ESC ESC" #'god-mode-isearch-disable god-mode-isearch-map)

  (defun god-mode-self-insert-on-meta ()
  "Copy of `god-mode-self-insert', except binding is M-key."
  (interactive)
  (let* ((initial-key (aref (this-command-keys-vector)
                            (- (length (this-command-keys-vector)) 1)))
         (key-string (concat "M-" (char-to-string initial-key)))
         (binding (key-binding (kbd key-string))))
    (unless binding (error "God: unknown key binding for `%s`" key-string))
    (setq this-original-command binding)
    (setq this-command binding)
    ;; `real-this-command' is used by emacs to populate
    ;; `last-repeatable-command', which is used by `repeat'.
    (setq real-this-command binding)
    (setq god-literal-sequence nil)
    (if (commandp binding t)
        (call-interactively binding)
      (execute-kbd-macro binding))))

  ;; bind symbols to M-?
  (dolist (i '("!" "@" "$" "%" "^" "&" "*" "{" "}"
               "<" ">" ";" ":" "|" "\\" "+" "=" "?" "]"))
    (define-key god-local-mode-map (kbd i) 'god-mode-self-insert-on-meta))

  ;; Bind some second level modifier keys with C- prefix for easier
  ;; god-mode access. Directly bind these to commands, instead of making
  ;; it a keyboard macro so that messages work in god-mode.
  (dolist (bindings
           '(("C-x" "0" "1" "2" "3" "9" "[" "]" "$" "," "." "?")
             ("M-g" "1" "2" "3" "4" "5" "6" "7" "8" "c" "n" "p")))
    (let ((prefix (car bindings))
          (chars (cdr bindings)))
      (dolist (i chars)
        (global-set-key (kbd (concat prefix " C-" i))
                        (key-binding (kbd (concat prefix " " i)))))))

  (defun z-god-mode-enabled-hook ()
    (z-god-set-state z-god-state)
    (set-cursor-type 'box)
    (setq-local z-god-saved-input-method current-input-method)
    (if current-input-method
      (deactivate-input-method)))
  (add-hook 'god-mode-enabled-hook 'z-god-mode-enabled-hook)

  (defun z-god-mode-disabled-hook ()
    (set-cursor-type 'bar)
    (setq z-god-mode-lighter
          '(:propertize (" " (:eval (or current-input-method-title "ε")) " ")
                        face (:background "#90E090" :foreground "black")))
    (setq z-god-state 'normal)
    (if z-god-saved-input-method
        (set-input-method z-god-saved-input-method)))
  (add-hook 'god-mode-disabled-hook 'z-god-mode-disabled-hook))
(add-hook 'after-init-hook 'god-mode-all)
