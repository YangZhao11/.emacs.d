; -*- coding: utf-8; lexical-binding: t -*-

;;; Code:
(eval-when-compile
  (require 'use-package)
  (require 'hydra))

(eval-after-load "quail/Latin-ltx"
  '(let ((quail-current-package (assoc "TeX" quail-package-alist)))
     (quail-define-rules
      ((append . t))
      ("^\\alpha" ?ᵅ)
      ("\\sqrt" ?√)
      ("\\rarr" ?→) ("\\larr" ?←) ("\\uarr" ?↑) ("\\darr" ?↓)
      ("\\Rarr" ?⇒) ("\\Larr" ?⇐) ("\\Uarr" ?⇑) ("\\Darr" ?⇓))))

(defun z-kill-buffer (arg)
  "Kill this buffer, or with ARG, call `kill-buffer` instead."
  (interactive "P")
  (call-interactively
   (if arg 'kill-buffer
     'kill-this-buffer)))
(bind-keys ("<C-M-backspace>" . backward-kill-sexp)
           ("C-x k"           . z-kill-buffer)
           ("C-M-o"           . up-list))

(use-package region-bindings-mode :diminish 'region-bindings-mode
  :commands (region-bindings-mode-enable))
(region-bindings-mode-enable)

(use-package easy-pair
  :commands (easy-pair-delete
             easy-pair-kill-inside)
  :bind (:map region-bindings-mode-map
              ("DEL" . easy-pair-delete)
              ("i" . easy-pair-kill-inside))
  :bind (("M-(" . easy-pair-barf)
         ("M-)" . easy-pair-slurp)))

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


(defvar z-align-alist
  '((?  "\\s-" 0)
    (?\" "\\s-\"" 0))
  "Specify regexp and spacing for z-align-char.")

(defun z-align-char (char beg end no-space)
  "Align CHAR in region specified by BEG and END.
With prefix arg (NO-SPACE), do not leave space before CHAR."
  (interactive "cAlign char: \nr\nP")
  (let* ((a (alist-get char z-align-alist))
         (regexp (concat  "\\(\\s-*\\)"
                          (if a (car a)
                            (regexp-quote (char-to-string char)))))
         (spacing (if a (cadr a)
                    (if no-space 0 1))))
    (align-regexp beg end regexp 1 spacing t)))
(bind-key "C-x ;" 'z-align-char)

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

  (defun easy-kill-delete-pairs ()
    (interactive)
    (easy-pair-delete (prefix-numeric-value current-prefix-arg)
                    (easy-kill-get start)
                    (easy-kill-get end))
    (when (eq (easy-kill-get start) (easy-kill-get end))
      (message "No selection, exit easy-kill")
      (easy-kill-exit)))

  (defun easy-kill-inside ()
    (interactive)
    (easy-pair-kill-inside (easy-kill-get start) (easy-kill-get end)))
  (put #'easy-kill-inside 'easy-kill-exit t)

  (add-to-list 'easy-kill-alist '(?p paragraph "\n"))
  (setq easy-kill-unhighlight-key " ")
  (setq easy-kill-try-things '(url email sexp line))

  (setq easy-kill-alist '((?w word       " ")
                          (?s sexp       "\n")
                          (?l list       "\n")
                          (?f filename   "\n")
                          (?d defun      "\n\n")
                          (?D defun-name " ")
                          (?n line       "\n") ;changed from ?e
                          (?b buffer-file-name)))
  (bind-keys
   :map easy-kill-base-map
   ("DEL" . easy-kill-delete-pairs)
   ("k"   . easy-kill-region)
   ("m"   . easy-kill-mark-region)
   ("i"   . easy-kill-inside)
   ("t"   . easy-kill-transpose)
   (";"   . easy-kill-comment-dwim)
   ("("   . easy-kill-wrap-region)
   (")"   . easy-kill-wrap-region)
   ("["   . easy-kill-wrap-region)
   ("]"   . easy-kill-wrap-region)
   ("{"   . easy-kill-wrap-region)
   ("}"   . easy-kill-wrap-region)
   ("\""  . easy-kill-wrap-region)
   ("'"   . easy-kill-wrap-region)
   ("\\"  . easy-kill-indent-region)))

(defun cycle-spacing-0 ()
  "Remove adjacent spaces, but undo if the command is issued a second time."
  (interactive) (cycle-spacing 0))

(defun toggle-selective-display (column)
  "Toggle selective display at COLUMN, defaulting to current column."
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
           ("C-x $"  . toggle-selective-display)
           ("M-?"    . completion-at-point))

(use-package string-inflection
  :bind (("M-U" . string-inflection-camelcase)
         ("M-C" . string-inflection-lower-camelcase)
         ("M-L" . string-inflection-underscore)))

(defun isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string. This is
useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))
(bind-keys :map isearch-mode-map
           ("M-RET" . isearch-exit-other-end)
           ("M-k"   . isearch-yank-word-or-char))
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
(bind-keys ("C-x /" . ediff-this-buffer))

(defun shrink-other-window-if-larger-than-buffer ()
    "Shrink other window if larger than buffer."
    (interactive)
    (shrink-window-if-larger-than-buffer
     (next-window (selected-window) nil nil)))
(bind-keys ("C-x _" . shrink-other-window-if-larger-than-buffer)
           ("C-x 9" . delete-other-windows-vertically))

(bind-key "C-z" nil)

;; F1 for help.
;; (bind-key "<f2>" #'eshell)
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
           ("M-0" . switch-to-next-buffer))

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
%s(ballotbox rainbow-delimiters-mode) rainbow-_d_elimiters  ^^ %s(ballotbox abbrev-mode \"∂\") _a_bbrev         %s(ballotbox outline-minor-mode) _o_utline-minor-mode ^^ %s(ballotbox lsp-mode \"£\") _l_sp-mode
%s(ballotbox rainbow-identifiers-mode) rainbow-_i_dentifiers ^^ %s(ballotbox auto-fill-function \"¶\") auto-_f_ill      %s(ballotbox view-mode) _v_iew-mode          ^^ %s(ballotbox flycheck-mode \"✔\") flychec_k_
%s(ballotbox beacon-mode) _b_eacon              ^^ %s(ballotbox visual-line-mode \"↵\") visual-lin_e_    %s(if (bound-and-true-p subword-mode) \",\" (if (bound-and-true-p superword-mode) \"²\" \"☐\")) sub_w_ord/super_W_ord
%s(ballotbox hi-lock-mode) _h_i-lock/_c_hanges      %s(ballotbox auto-revert-mode \"↻\") auto-_r_evert    %s(ballotbox flyspell-mode \"⍹\") fl_y_spell/_p_rog       %s(ballotbox which-function-mode) which-f_u_nc
%s(ballotbox whitespace-mode \"␣\") white_s_pace/_t_railing  %s(ballotbox display-line-numbers-mode) line _n_um
"
  ("a"    abbrev-mode)
  ("b"    beacon-mode)
  ("c"    highlight-changes-mode)
  ("d"    rainbow-delimiters-mode)
  ("e"    visual-line-mode)
  ("f"    auto-fill-mode)
  ("h"    hi-lock-mode)
  ("i"    rainbow-identifiers-mode)
  ("k"    flycheck-mode)
  ("l"    lsp-mode)
  ("p"    flyspell-prog-mode)
  ("n"    display-line-numbers-mode)
  ("o"    outline-minor-mode)
  ("r"    auto-revert-mode)
  ("s"    whitespace-mode)
  ("t"    toggle-show-trailing-whitespace)
  ("u"    which-function-mode)
  ("v"    view-mode)
  ("w"    subword-mode)
  ("W"    superword-mode)
  ("y"    flyspell-mode)
  ("SPC"  nil)
)
(bind-key "C-x t" 'hydra-toggle/body)
(diminish 'abbrev-mode " ∂")
(diminish 'auto-fill-function " ¶")
(diminish 'visual-line-mode " ↵")
(setq display-line-numbers-type 'relative)


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
              ("SPC" . avy-goto-line)
              ("C-j" . avy-show-dispatch))
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
    "Jump to the currently visible CHAR in the current line after point."
    (interactive (list (read-char "char: " t)))
    (avy-with avy-goto-char
      (avy--generic-jump
       (regexp-quote (string char))
       avy-all-windows
       avy-style
       (1+ (point))
       (line-end-position))))
  (defun avy-backward-char-in-line (char)
    "Jump to the currently visible CHAR in the current line before point."
    (interactive (list (read-char "char: " t)))
    (avy-with avy-goto-char
      (avy--generic-jump
       (regexp-quote (string char))
       avy-all-windows
       avy-style
       (line-beginning-position)
       (point))))

  (defun z-goto-char (char &optional arg)
  "Call `avy-goto-char' or `avy-goto-subword-1', but respect bindings
in `ctl-j-map' first."
  (interactive (list (read-char "C-j ")
                     current-prefix-arg))
  (let ((act (lookup-key ctl-j-map (char-to-string char))))
    (cond (act (call-interactively act))
          ((string-match-p "[[:alpha:]]" (char-to-string char))
           (avy-goto-subword-1 char arg))
          ('t (avy-goto-char char arg)))))

  (defun avy-show-dispatch ()
    "show help for using `avy-dispatch-alist'"
    (interactive)
    (message "%s"
             (mapconcat 'z-replace-hotkey
                        '("_x_:kill" "_X_:kill-stay" "_t_eleport" "_m_ark"
                          "_n_:copy" "_y_ank" "_i_spell")
                        "  "))))

(defun z-replace-hotkey (x)
  "replace _x_ constructs with propertized text"
  (replace-regexp-in-string "_\\(.\\)_"
    (lambda (y) (propertize (match-string 1 y) 'face 'hydra-face-red))
    x))


(use-package ace-window :ensure :defer 6
  :bind* (("M-j" . z-ace-window)
          ("M-J" . ace-swap-window))
  :config
  (setq aw-scope 'frame
        aw-background nil
        aw-keys '(?j ?k ?d ?f ?g ?h ?s ?l ?a ?\;))
  (setq aw-dispatch-alist
        '((?x aw-delete-window " Ace - Delete Window" "_x_:delete")
    (?m aw-swap-window " Ace - Swap Window" "_m_:swap")
    (?M aw-move-window " Ace - Move Window" "_M_ove")
    (?c aw-split-window-fair " Ace - Split Fair Window" "_c_:split fair")
    (?v aw-split-window-vert " Ace - Split Vert Window" "_v_ert split")
    (?b aw-split-window-horz " Ace - Split Horz Window" "_b_:split h")
    (?i delete-other-windows " Ace - Delete Other Windows" "max_i_mize")))

  (defun z-ace-window (arg)
  "Select a window.
Perform an action based on ARG described below.

Prefixed with \\[universal-argument], show dispatch action."
  (interactive "P")
  (if arg
      (progn
        (ace-window-show-dispatch)
        (let ((aw-dispatch-always 't))(ace-select-window)))
    (ace-select-window)))
  (defun ace-window-show-dispatch ()
    (message "%s"
     (mapconcat (lambda (x) (z-replace-hotkey (cadddr x)))
      aw-dispatch-alist "  "))))

(use-package zap-to-char-dwim
  :bind (("M-z" . zap-to-char-dwim)
         ("M-Z" . zap-back-to-char-dwim)))

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
  (setq ivy-switch-buffer-faces-alist '((dired-mode . dired-directory)
                                        (org-mode . org-level-4)))
  (setq ivy-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
          "^\\*LV\\*"
           "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
           "\\[r\\]\\(<[0-9]+>\\)?$" "\\[fundamental\\]\\(<[0-9]+>\\)?$"
           "_region_" " output\\*$" "^TAGS$" "^\*Ido"))

  (defvar z-ivy-switch-buffer-padding 50
    "padding for g3 client name column")
  (defun z-ivy-switch-buffer-transformer (bufname)
    "Add g3 client name as a separate column"
    (let* ((buf (get-buffer bufname))
           (filename (and buf (buffer-file-name buf)))
           (g3 (and filename
                    (s-contains-p "/google3/" filename)
                    (replace-regexp-in-string
                     ".*/\\([^/]+\\)/google3/.*" "\\1" filename)))
          (s bufname))
      (if g3 (setq s (s-concat (s-pad-right z-ivy-switch-buffer-padding " " s) " "
                               (propertize g3 'face 'ivy-virtual))))
      (if buf s bufname)))
  (ivy-set-display-transformer 'ivy-switch-buffer 'z-ivy-switch-buffer-transformer)

  (bind-keys :map ivy-minibuffer-map
             ("M-s o" . ivy-occur)
             ("C-j" . ivy-avy)
             ("C-'" . ivy-alt-done)
             ("M-k" . ivy-yank-word)
             ("M-m" . ivy-restrict-to-matches)
             ("<home>" . hydra-ivy/body))

  (defhydra hydra-ivy-occur (:color pink :hint nil)
    "
_k_↑  _h_←   tg _c_alling   _f_:press   _g_:revert
_j_↓  _l_→   set _a_ction   _RET_:go    _o_ther    _q_uit
"
    ("SPC" nil)
    ("RET" ivy-occur-press-and-switch :exit t)
    ("a" ivy-occur-read-action)
    ("c" ivy-occur-toggle-calling)
    ("f" ivy-occur-press)
    ("g" ivy-occur-revert-buffer)
    ("h" backward-char)
    ("j" ivy-occur-next-line)
    ("k" ivy-occur-previous-line)
    ("l" forward-char)
    ("o" ivy-occur-dispatch :exit t)
    ("q" quit-window :exit t))
  (bind-keys :map ivy-occur-mode-map
             ("SPC" . hydra-ivy-occur/body)))

(use-package counsel :defer 4
  :bind (([remap find-file] . counsel-find-file)
         ("C-x 8 8" . counsel-unicode-char)
         ("C-x b" . counsel-bookmark)
         ("C-x f" . counsel-file-jump)  ; set-fill-column
         ("C-x d" . counsel-dired-jump)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-SPC" . counsel-mark-ring)
         ("M-s M-s" . counsel-grep-or-swiper)
         ("M-s i" . counsel-imenu))
  :bind (:map ctl-j-map
              ("C-i" . counsel-imenu))
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
            ((functionp isearch-regexp-function)
             (funcall isearch-regexp-function isearch-string))
            (isearch-regexp-function (word-search-regexp isearch-string))
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
(defvar-local z-god-saved-input-method nil
  "Saved input method before god-mode")
(defvar-local z-god-saved-view-mode nil
  "Saved view-mode before god-mode")

(defconst z-lighter-emacs
  '(:propertize (" " (:eval (or current-input-method-title "ε")) " ")
                face (:background "#90E090" :foreground "black")))
(defconst z-lighter-view
  '(:propertize " ν "
                face (:background "#E8BB74" :foreground "black")))
(defvar z-lighter
  '(:eval (if god-local-mode
              z-god-mode-lighter
            (if view-mode z-lighter-view
              z-lighter-emacs))))

(defun set-cursor-type (type)
  "Set cursor to TYPE for all frames."
  (dolist (f (frame-list))
    (modify-frame-parameters f `((cursor-type . ,type)))))

(use-package god-mode :ensure
  :bind (("<home>" . god-mode-all))
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
  (bind-keys :map isearch-mode-map
             ("<home>" . god-mode-isearch-activate))
  (bind-keys :map god-mode-isearch-map
             ("<home>" . god-mode-isearch-disable))

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
               "<" ">" ":" "|" "\\" "+" "=" "?" "]"))
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
    ;; somehow this hook can be called multiple times on a buffer,
    ;; which messes up saving states here. Maybe consider using
    ;; post-command-hook to run this once.
    (z-god-set-state z-god-state)
    (set-cursor-type 'box)
    (setq-local z-god-saved-input-method current-input-method)
    (if current-input-method
        (deactivate-input-method))
    (setq-local z-god-saved-view-mode view-mode)
    (if view-mode
        (view-mode -1)))
  (add-hook 'god-mode-enabled-hook 'z-god-mode-enabled-hook)

  (defun z-god-mode-disabled-hook ()
    (set-cursor-type 'bar)
    (setq z-god-state 'normal)
    (if z-god-saved-input-method
        (set-input-method z-god-saved-input-method))
    (if z-god-saved-view-mode
        (view-mode 1)))
  (add-hook 'god-mode-disabled-hook 'z-god-mode-disabled-hook))
(add-hook 'after-init-hook 'god-mode-all)
