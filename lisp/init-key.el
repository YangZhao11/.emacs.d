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

(use-package kmarcro
  :config
  (setcdr (assq 'defining-kbd-macro minor-mode-alist)
          '((:propertize " ●" face (:foreground "#D04020")
                         help-echo "Recording keyboard macro"))))

(defhydra hydra-ctl-x-r (:color blue :hint nil)
  "
^^Rectangle^^‗‗‗‗‗‗‗‗‗‗‗‗‗^^   ^^Register^^‗‗save‗‗‗‗‗‗‗‗‗^^  ^^Bookmark‗‗‗‗‗‗
_c_lear     _N_umber-lines^^   _+_: inc^^    _SPC_: point     _m_: set
_d_elete    _o_pen  s_t_ring   _i_nsert^^    _f_rameset       _b_: jump
_k_ill      _y_ank^^           _j_ump^^      _w_indow-config  _l_ist
M-w:copy^^  _r_egister^^       _x_/_s_:copy  _n_umber         _M_: no-overwrite
"
  ("SPC" point-to-register)
  ("+" increment-register)
  ("M" bookmark-set-no-overwrite)
  ("N" rectangle-number-lines)
  ("b" bookmark-jump)
  ("c" clear-rectangle)
  ("d" delete-rectangle)
  ("f" frameset-to-register)
  ("g" insert-register)
  ("i" insert-register)
  ("j" jump-to-register)
  ("k" kill-rectangle)
  ("l" bookmark-bmenu-list)
  ("m" bookmark-set)
  ("n" number-to-register)
  ("o" open-rectangle)
  ("r" copy-rectangle-to-register)
  ("s" copy-to-register)
  ("t" string-rectangle)
  ("w" window-configuration-to-register)
  ("x" copy-to-register)
  ("y" yank-rectangle)
  )
(bind-key "C-x r ?" 'hydra-ctl-x-r/body)

(defhydra hydra-ctl-x-v (:color blue :hint nil)
  "
_+_:update   ch_a_nge log     print _l_og   _b_ackend   _h_istory    _m_erge
_=_:diff     log _I_ncoming   root _L_og    _P_ush      _d_ir        reg_i_ster   _r_etrieve tag
root-_D_iff  log _O_utgoing   _~_:revision  i_G_nore    _g_:annotate _u_:revert
"
  ("+" vc-update)
  ("=" vc-diff)
  ("D" vc-root-diff)
  ("G" vc-ignore)
  ("I" vc-log-incoming)
  ("L" vc-print-root-log)
  ("O" vc-log-outgoing)
  ("P" vc-push)
  ("a" vc-update-change-log)
  ("b" vc-switch-backend)
  ("d" vc-dir)
  ("g" vc-annotate)
  ("h" vc-region-history)
  ("i" vc-register)
  ("l" vc-print-log)
  ("m" vc-merge)
  ("r" vc-retrieve-tag)
  ("s" vc-create-tag)
  ("u" vc-revert)
  ("v" vc-next-action)
  ("x" vc-delete-file)
  ("~" vc-revision-other-window)
  )
(bind-key "C-x v ?" 'hydra-ctl-x-v/body)

(defun z-kill-buffer (arg)
  "Kill this buffer, or with ARG, call `kill-buffer' instead."
  (interactive "P")
  (if arg (call-interactively 'kill-buffer)
    (kill-buffer)))
(bind-keys ("<C-M-backspace>" . backward-kill-sexp)
           ("C-x k"           . z-kill-buffer)
           ("C-M-o"           . up-list))

(use-package region-bindings-mode
  :diminish 'region-bindings-mode
  :commands (region-bindings-mode-enable))
(region-bindings-mode-enable)

(use-package easy-pair
  :commands (easy-pair-delete
             easy-pair-kill-inside)
  :bind (:map region-bindings-mode-map
              ("DEL" . easy-pair-delete)
              ("i" . easy-pair-kill-inside))
  :bind (("M-7" . easy-pair-backward-slurp)
         ("M-8" . easy-pair-backward-barf)
         ("M-(" . easy-pair-barf)
         ("M-)" . easy-pair-slurp)))

(use-package anchored-transpose :ensure
    :bind (:map region-bindings-mode-map
                ("C-t" . anchored-transpose)))

(use-package shell
  :bind ("<f6>" . shell))

(use-package z-misc
  :bind
  ("C-x ;" . z-align-char)
  ("C-x $" . z-toggle-selective-display)
  ("C-x /" . z-ediff-this-buffer)
  ("C-x _" . z-shrink-other-window-if-larger-than-buffer))

(use-package easy-kill :ensure
  :functions (easy-kill-mark-region easy-kill-exit)
  :bind ([remap kill-ring-save] . easy-kill)
  :config

  (easy-kill-defun easy-kill-transpose ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'anchored-transpose)))

  (easy-kill-defun easy-kill-wrap-region ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'self-insert-command)))

  (easy-kill-defun easy-kill-indent-region ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'indent-region)))

  (easy-kill-defun easy-kill-comment-dwim ()
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively #'comment-dwim)))

  (defun easy-kill-delete-pairs ()
    (interactive)
    (easy-pair-delete (prefix-numeric-value current-prefix-arg)
                    (easy-kill-get start)
                    (easy-kill-get end))
    (when (eq (easy-kill-get start) (easy-kill-get end))
      (message "No selection, exit easy-kill")
      (easy-kill-exit)))

  (easy-kill-defun easy-kill-inside ()
    (interactive)
    (easy-pair-kill-inside (easy-kill-get start) (easy-kill-get end)))
  (put #'easy-kill-inside 'easy-kill-exit t)

  (setq easy-kill-unhighlight-key (kbd "SPC"))
  (setq easy-kill-try-things '(url email sexp line))

  (setq easy-kill-alist '((?w word       " ")
                          (?s sexp       "\n")
                          (?l list       "\n")
                          (?f filename   "\n")
                          (?d defun      "\n\n")
                          (?D defun-name " ")
                          (?p paragraph "\n")
                          (?n line       "\n") ;changed from ?e
                          (?b buffer-file-name)))
  (bind-keys
   :map easy-kill-base-map
   ("<f1>" . easy-kill-help)
   ("C-h" . easy-kill-help)
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

(use-package minibuffer
  :bind ("M-?" . completion-at-point))

(use-package simple
  :commands (cycle-spacing-0)
  :bind (("M-SPC"       . cycle-spacing)
         ("M-\\"        . cycle-spacing-0)
         ("M-c"         . capitalize-dwim)
         ("M-l"         . downcase-dwim)
         ("M-u"         . upcase-dwim)
         ("<XF86Eject>" . keyboard-escape-quit))
  :config
  (defun cycle-spacing-0 ()
    "Remove adjacent spaces, but undo if the command is issued a second time."
    (interactive)
    (cycle-spacing 0))
  (diminish 'next-error-follow-minor-mode " ⇅")
  (bind-keys :map special-mode-map
             ("j" . scroll-up-command)
             ("k" . scroll-down-command)
             ("[" . backward-page)
             ("]" . forward-page)
             ("x" . god-mode-self-insert)
             ("c" . god-mode-self-insert)))

(use-package string-inflection
  :bind (("M-U" . string-inflection-upcase)
         ("M-C" . string-inflection-camelcase-cycle)
         ("M-L" . string-inflection-underscore-cycle))
  :config
  (defun string-inflection-camelcase-cycle ()
  "fooBar => FooBar => fooBar"
  (interactive)
  (string-inflection-insert
   (string-inflection-camelcase-cycle-function
    (string-inflection-get-current-word))))

  (defun string-inflection-camelcase-cycle-function (str)
  "fooBar => FooBar => fooBar"
  (cond
   ((string-inflection-upper-camelcase-p str)
    (string-inflection-lower-camelcase-function str))
   (t
    (string-inflection-upper-camelcase-function str))))

  (defun string-inflection-underscore-cycle ()
  "foo_bar => foo-bar => foo_bar"
  (interactive)
  (string-inflection-insert
   (string-inflection-underscore-cycle-function
    (string-inflection-get-current-word))))

  (defun string-inflection-underscore-cycle-function (str)
  "foo_bar => foo-bar => foo_bar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-kebab-case-function str))
   (t
    (string-inflection-underscore-function str)))))

(use-package grep
  :bind (("M-s g"   . grep)
         ("M-s M-g" . rgrep)))

(use-package replace
  :bind ("M-s M-o" . multi-occur-in-matching-buffers))

(use-package isearch
  :config
  (defun isearch-exit-other-end ()
    "Exit isearch, but at the other end of the search string. This is
useful when followed by an immediate kill."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))
  (bind-keys :map isearch-mode-map
             ("M-RET" . isearch-exit-other-end)
             ("M-k"   . isearch-yank-word-or-char))

  (setcdr (assq 'isearch-mode minor-mode-alist)
          '((:eval (if isearch-forward " »" " «")))))


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

(bind-keys ("C-x 9" . delete-other-windows-vertically))
(bind-key "C-z" nil)

;; F1 for help.
;; (bind-key "<f2>" #'eshell)
;; F3 and F4 for macros
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

(defhydra hydra-resize-window (:color pink :hint nil)
    "
use arrow keys or:  _{_ _}_ horizontal   _[_ _]_ vertical
"
    ("{" shrink-window-horizontally)
    ("<left>" shrink-window-horizontally)
    ("}" enlarge-window-horizontally)
    ("<right>" enlarge-window-horizontally)
    ("[" shrink-window)
    ("<up>" shrink-window)
    ("]" enlarge-window)
    ("<down>" enlarge-window)
    ("SPC" nil)
    ("<f10>" nil))
(bind-keys ("<f10>" . hydra-resize-window/body)
          ("<f11>" . shrink-window)
          ("<f12>" . enlarge-window)
          ("M-9" . previous-buffer)
          ("M-0" . next-buffer)
          ("C-x 4 o" . display-buffer))

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
%s(ballotbox rainbow-delimiters-mode) rainbow-_d_elimiters  ^^ %s(ballotbox abbrev-mode \"∂\") _a_bbrev         %s(ballotbox outline-minor-mode) _o_utline-minor-mode ^^ %s(ballotbox beacon-mode) _b_eacon
%s(ballotbox rainbow-identifiers-mode) rainbow-_i_dentifiers ^^ %s(ballotbox auto-fill-function \"¶\") auto-_f_ill      %s(ballotbox view-mode) _v_iew-mode          ^^ %s(ballotbox flycheck-mode \"✔\") flychec_k_
%s(ballotbox rainbow-mode) _R_ainbow              %s(ballotbox visual-line-mode \"↵\") visual-lin_e_    %s(if (bound-and-true-p subword-mode) \",\" (if (bound-and-true-p superword-mode) \"²\" \"☐\")) sub_w_ord/super_W_ord   %s(ballotbox electric-quote-mode) elec-_'_
%s(ballotbox hi-lock-mode) _h_i-lock/_c_hanges      %s(ballotbox auto-revert-mode \"↻\") auto-_r_evert    %s(ballotbox flyspell-mode \"⍹\") fl_y_spell/_p_rog       %s(ballotbox which-function-mode) which-f_u_nc
%s(ballotbox whitespace-mode \"␣\") white_s_pace/_t_railing  %s(ballotbox display-line-numbers-mode) line _n_um       %s(ballotbox flymake-mode) fly_m_ake
"
  ("'"    electric-quote-mode)
  ("a"    abbrev-mode)
  ("b"    beacon-mode)
  ("c"    highlight-changes-mode)
  ("d"    rainbow-delimiters-mode)
  ("e"    visual-line-mode)
  ("f"    auto-fill-mode)
  ("h"    hi-lock-mode)
  ("i"    rainbow-identifiers-mode)
  ("k"    flycheck-mode)
  ("m"    flymake-mode)
  ("p"    flyspell-prog-mode)
  ("n"    display-line-numbers-mode)
  ("o"    outline-minor-mode)
  ("r"    auto-revert-mode)
  ("R"    rainbow-mode)
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

(use-package rainbow-mode :diminish rainbow-mode)


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
  (defun z-all-frames-to-messages-buffer ()
    "Make all frames display the *Messages* buffer, only after storing
current frame configuration to register 6."
    (interactive)
    (frameset-to-register ?6)
    (dolist (f (frame-list))
      (let ((w (frame-first-window f)))
        (delete-other-windows w)
        (set-window-buffer w "*Messages*")))
    (message "All frames to *Messages*. Originals stored in register 6."))

  (bind-keys :map register-channel-mode-map
             ("M-g 4" . register-channel-save-window-configuration)
             ("M-g 5" . register-channel-save-window-configuration)
             ("M-g 6" . z-all-frames-to-messages-buffer)
             ("M-g 7") ("M-g 8") ("M-7") ("M-8")))

(defvar ctl-j-map (make-sparse-keymap)
  "Keymap behind C-j. Called by `z-goto-char'.")

(use-package goto-chg :ensure
  :bind (("M-i" . goto-last-change)
         ("M-I" . goto-last-change-reverse)))

(use-package avy :ensure :defer 5
  :bind ("C-j" . z-goto-char)
  :bind (:map ctl-j-map
              ("SPC" . avy-goto-line)
              ("RET" . avy-show-dispatch)
              ("TAB" . avy-yank-word-1)
              ("C-e" . avy-goto-end-of-line))
  :bind (("M-," . avy-backward-char-in-line)
         ("M-." . avy-forward-char-in-line))
  :config
  (require 'subword)
  (setq avy-styles-alist '((avy-goto-char . de-bruijn))
        avy-keys
        '(?s ?d ?f ?g ?h ?j ?k ?l ?w ?e ?r ?u ?i ?o)
        avy-subword-extra-word-chars nil
        avy-orders-alist '((avy-goto-char . avy-order-closest)
                           (avy-goto-subword-1 . avy-order-closest)))
  (eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "C-j") #'avy-isearch))


  (defun avy-yank-word-1 (char &optional arg beg end symbol)
    "Like `avy-goto-word-1', but yank instead."
    (interactive (list (read-char "char: " t)
                       current-prefix-arg))
    (avy-with avy-goto-word-1
      (setq avy-action 'avy-action-yank)
      (let* ((str (string char))
             (regex (cond ((string= str ".")
                           "\\.")
                          ((and avy-word-punc-regexp
                                (string-match avy-word-punc-regexp str))
                           (regexp-quote str))
                          ((<= char 26)
                           str)
                          (t
                           (concat
                            (if symbol "\\_<" "\\b")
                            str)))))
        (avy--generic-jump regex arg avy-style beg end))))

  (defun avy-forward-char-in-line (char)
    "Jump to the currently visible CHAR in the current line after point."
    (interactive (list (read-char "char: " t)))
    (avy-with avy-goto-char
      (avy--generic-jump
       (regexp-quote (string char))
       nil
       avy-style
       (1+ (point))
       (line-end-position))))
  (defun avy-backward-char-in-line (char)
    "Jump to the currently visible CHAR in the current line before point."
    (interactive (list (read-char "char: " t)))
    (avy-with avy-goto-char
      (avy--generic-jump
       (regexp-quote (string char))
       nil
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
    "Show help for using `avy-dispatch-alist'"
    (interactive)
    (message "%s"
       (mapconcat
        (lambda (x)
          (concat (propertize (char-to-string (car x)) 'face 'hydra-face-red)
                  ":" (replace-regexp-in-string "avy-action-" ""
                                                (symbol-name (cdr x)))))
        avy-dispatch-alist
        "  "))))

(use-package ace-window :ensure :defer 6
  :bind* (("M-j" . z-ace-window)
          ("M-J" . ace-swap-window))
  :config
  (setq aw-scope 'frame
        aw-background nil
        aw-keys '(?j ?d ?k ?f ?g ?h ?s ?l ?a ?\;))

  (defun z-ace-window (arg)
  "Select a window.
Perform an action based on ARG described below.

Prefixed with \\[universal-argument], show dispatch action."
  (interactive "P")
  (if arg
      (let ((aw-dispatch-always 't))
        (aw-show-dispatch-help))
    (ace-select-window))))

(use-package zap-to-char-dwim
  :bind (("M-z" . zap-to-char-dwim)
         ("M-Z" . zap-back-to-char-dwim)))

(use-package find-dired
  :bind ("C-x d" . find-name-dired))

(use-package imenu-anywhere
  :bind ("M-s M-i" . ivy-imenu-anywhere))

(use-package recentf
  :config
  (setq recentf-max-saved-items 80)
  (setq recentf-exclude '("/elpa/")))

(use-package ivy :diminish ""
  :bind (("M-o" . ivy-switch-buffer)
         ("M-s M-d" . ivy-resume)
         ("M-g v" . ivy-push-view)
         ("M-g M-v" . ivy-pop-view))
  :config
  (ivy-mode 1)
  (setq ivy-count-format "")
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers 't)
  (setq ivy-use-selectable-prompt 't)
  (setq ivy-switch-buffer-faces-alist '((dired-mode . dired-directory)
                                        (org-mode . org-level-4)))
  (setq ivy-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
          "^\\*LV\\*" "^\\*Ilist\\*" "^\\*:Buffers:\\*"
          "^:"        ; see `dired-sidebar-buffer-name'
           "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
           "\\[r\\]\\(<[0-9]+>\\)?$" "\\[fundamental\\]\\(<[0-9]+>\\)?$"
           "_region_" " output\\*$" "^TAGS$" "^\*Ido"))

  (defun z-ivy-repo (bufname)
    (let* ((buf (get-buffer bufname)))
      (and buf (cdr (z-project buf)))))

  (defvar z-ivy-switch-buffer-padding 50
    "padding for g3 client name column")
  (defun z-ivy-switch-buffer-transformer (bufname)
    "Add project name as a separate column"
    (if-let* ((repo (z-ivy-repo bufname)))
        (s-concat (s-pad-right z-ivy-switch-buffer-padding " " bufname) " "
                  (propertize repo 'face 'ivy-virtual))
      bufname))
  (ivy-set-display-transformer 'ivy-switch-buffer 'z-ivy-switch-buffer-transformer)

  (bind-keys :map ivy-minibuffer-map
             ("M-s o" . ivy-occur)
             ("C-j" . ivy-avy)
             ("C-c C-c" . ivy-toggle-calling)
             ("C-'" . ivy-alt-done)
             ("M-k" . ivy-yank-word)
             ("M-s ." . ivy-yank-symbol)
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
             ("n" . ivy-occur-next-line)
             ("p" . ivy-occur-previous-line)
             ("x" . god-mode-self-insert)
             ("SPC" . hydra-ivy-occur/body)))

(use-package counsel :defer 4
  :bind (([remap find-file] . counsel-find-file)
         ("C-x 8 8" . counsel-unicode-char)
         ("C-x b" . counsel-bookmark)
         ("C-x f" . counsel-file-jump)  ; set-fill-column
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("M-s M-s" . counsel-grep-or-swiper)
         ("M-s m" . counsel-mark-ring)
         ("M-s i" . counsel-imenu)
         ("M-s r" . counsel-jump-to-register))
  :bind (:map help-map
              ("v" . counsel-describe-variable)
              ("f" . counsel-describe-function)
              ("S" . counsel-info-lookup-symbol))
  :config
  (setq counsel-find-file-ignore-regexp
        "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)\\|\\(\\`\\.\\)")

  (defun counsel-jump-to-register ()
    "Jump to register with markers"
    (interactive)
  (ivy-read "Register: "
            (mapcar (lambda (register-alist-entry)
                      (if (markerp (cdr register-alist-entry))
                          (let* ((mk (cdr register-alist-entry))
                                 (buf (marker-buffer mk)))
                            (concat (char-to-string (car register-alist-entry))
                                    "    "
                                    (if (null buf)
                                        "in no buffer"
                                      (concat (buffer-name buf) ":"
                                              (number-to-string (marker-position mk))))))))
                    register-alist)
            :require-match t
            :initial-input "^"
            :history 'counsel-jump-toregister
            :caller 'counsel-jump-toregister
            :action (lambda (register-line)
                      (jump-to-register (elt register-line 0)))
            )))

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

;; --------------------------------------------------
(defvar z-god-mode-lighter "")
(defvar-local z-god-saved-input-method nil
  "Saved input method before god-mode")
(defvar-local z-god-saved-view-mode nil
  "Saved view-mode before god-mode")

(defun set-cursor-type (spec)
  "Set cursor type for current frame. This also works for
terminals with support for setting cursor type.

SPEC could be `box', 'bar', or `hbar'."
  (cond
   ((display-graphic-p)
    (modify-frame-parameters nil `((cursor-type . ,spec))))
   ((frame-terminal)
    (let* ((shape (or (car-safe spec) spec))
           (param (cond ((eq shape 'bar) "6")
                        ((eq shape 'hbar) "4")
                        (t "2"))))
      (send-string-to-terminal
       (concat "\e[" param " q"))))))

(defun set-cursor-type-all-frames (spec)
  "Set cursor to SPEC for all frames."
  (dolist (f (frame-list))
    (with-selected-frame f (set-cursor-type spec))))

(define-minor-mode mortal-mode
  "Allow temporary departures from god-mode."
  :keymap '(([return] . (lambda ()
                          "Exit mortal-mode and resume god mode." (interactive)
                          (god-local-mode-resume)
                          (mortal-mode 0))))
  (when mortal-mode
    (condition-case nil
        (progn (barf-if-buffer-read-only)
               (god-local-mode-pause))
      (buffer-read-only
       (mortal-mode 0)
       (user-error "Buffer is read-only.")))))

(use-package god-mode :ensure
  :bind (("<home>" . god-mode-all))
  :diminish god-local-mode
  :config

  (require 'god-mode-isearch)
  (bind-keys :map isearch-mode-map
             ("<home>" . god-mode-isearch-activate))
  (bind-keys :map god-mode-isearch-map
             ("<home>" . god-mode-isearch-disable))

  (setq god-mod-alist '((nil . "C-") ("g" . "M-") ("h" . "C-M-")))
  (setq god-exempt-major-modes nil
        god-exempt-predicates nil)

  ;; Avoid remapped self-insert-command
  (defalias 'true-self-insert-command 'self-insert-command)

  ;; A low priority map that takes precedence after local maps.
  (setq god-mode-low-priority-map (make-sparse-keymap))
  (defun god-mode-low-priority ()
    "Honor local binding first, then use `god-mode-low-priority-map'."
    (interactive)
    (let* ((keys (this-command-keys))
           (binding (or (local-key-binding keys)
                        (lookup-key god-mode-low-priority-map keys))))
      (unless binding (error "God: unknown binding for `%s'"  keys))
      (cond ((commandp binding t)
             (setq this-original-command binding)
             (setq this-command binding)
             ;; `real-this-command' is used by emacs to populate
             ;; `last-repeatable-command', which is used by `repeat'.
             (setq real-this-command binding)
             (call-interactively binding))
            ((keymapp binding)
             ;; help-form does not work, but actual key is.
             (setq help-form `(describe-vector ,(vector binding)))
             (set-transient-map binding nil (lambda () (setq help-form nil))))
            (t (execute-kbd-macro binding)))))

  (bind-keys :map god-mode-low-priority-map
             ("q" . quoted-insert)
             ("[" . backward-sexp)
             ("]" . forward-sexp)
             ("(" . true-self-insert-command)
             (")" . true-self-insert-command)
             ("`" . next-error)
             ("#" . server-edit))

  (bind-keys :map god-local-mode-map
             ("i" . mortal-mode)
             ("z" . repeat))

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

  ;; bind symbols to M-? with low priority
  (dolist (i '("~" "!" "@" "$" "%" "^" "&" "*" "{" "}"
               "<" ">" ":" "|" "\\" "+" "=" "?"))
    (define-key god-mode-low-priority-map (kbd i)
      'god-mode-self-insert-on-meta))

  (dolist (b (cdr god-mode-low-priority-map))
    (define-key god-local-mode-map (char-to-string (car b))
      'god-mode-low-priority))

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

  (defun god-mode-call-with-prefix (prefix)
    "Call god-mode key binding, as if prefix and literal key are in effect"
    (interactive)
    (let ((god-literal-sequence t))
      (call-interactively (god-mode-lookup-key-sequence nil prefix))))
  (global-set-key (kbd "C-x C-4")
                  (lambda () (interactive) (god-mode-call-with-prefix "C-x 4")))
  (global-set-key (kbd "C-x C-5")
                  (lambda () (interactive) (god-mode-call-with-prefix "C-x 5")))
  (global-set-key (kbd "C-x C-6")
                  (lambda () (interactive) (god-mode-call-with-prefix "C-x 6")))
  (global-set-key (kbd "C-x C-8")
                  (lambda () (interactive) (god-mode-call-with-prefix "C-x 8")))

  (defun z-god-mode-enabled-hook ()
    ;; somehow this hook can be called multiple times on a buffer,
    ;; which messes up saving states here. Maybe consider using
    ;; post-command-hook to run this once.
    (mortal-mode 0)
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
    (if z-god-saved-input-method
        (set-input-method z-god-saved-input-method))
    (if z-god-saved-view-mode
        (view-mode 1)))
  (add-hook 'god-mode-disabled-hook 'z-god-mode-disabled-hook))
(add-hook 'after-init-hook 'god-mode-all)
