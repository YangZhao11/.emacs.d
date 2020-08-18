; -*- coding: utf-8; lexical-binding: t -*-

;;; Code:
(eval-when-compile
  (require 'use-package)
  (require 'hydra)
  (require 'subr-x))

(eval-after-load "quail/Latin-ltx"
  '(let ((quail-current-package (assoc "TeX" quail-package-alist)))
     (quail-define-rules
      ((append . t))
      ("^\\alpha" ?ᵅ)
      ("\\sqrt" ?√)
      ("\\rarr" ?→) ("\\larr" ?←) ("\\uarr" ?↑) ("\\darr" ?↓)
      ("\\Rarr" ?⇒) ("\\Larr" ?⇐) ("\\Uarr" ?⇑) ("\\Darr" ?⇓))))

(defun z-setup-terminal ()
  ;; This is not on by default, but needed for `read-key' to read M-z
  ;; as one keypress on terminal.
  (cl-loop for c from ?! to ?~ do
           (when (not (memq c '(?\[ ?O)))
    (define-key input-decode-map
      (kbd (concat "ESC " (char-to-string c)))
      (kbd (concat "M-" (char-to-string c))))))
  (cl-loop for c from ?a to ?z do
    (define-key input-decode-map
      (kbd (concat "ESC C-" (char-to-string c)))
      (kbd (concat "C-M-" (char-to-string c)))))

  ;; CSI u style coding for C-. and C-M-. keys
  (cl-loop for c from ?! to ?~ do
           (let* ((s (format "%d" c))
                  (spaced (replace-regexp-in-string "\\([0-9]\\)" "\\1 " s)))
             (cl-loop for b in
                      '(("5" . "C-") ("7" . "C-M-")) do
                      (define-key input-decode-map
                        (kbd (concat "M-[ " spaced "; " (car b) " u"))
                        (kbd (concat (cdr b) (char-to-string c))))))))
(add-hook 'tty-setup-hook #'z-setup-terminal)

(use-package kmacro
  :config
  (setcdr (assq 'defining-kbd-macro minor-mode-alist)
          '((:propertize " ●" face (:foreground "#D04020")
                         help-echo "Recording keyboard macro"))))

(defhydra hydra-ctl-x-r (:color blue :hint nil)
  "
┌─────^^Rectangle^^^^──────┐   ┌^^Register^^─save^^────────┐  ┌─^^Bookmark────┐
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
(bind-key "C-x k" 'z-kill-buffer)

;; lisp is not a package
;; (use-package lisp)
(defun backward-down-list (arg)
  "Go down backwards"
  (interactive "^p")
  (down-list (- (or arg 1))))

(bind-keys ("<C-M-backspace>" . backward-kill-sexp)
           ("C-M-o"           . up-list)
           ("C-M-j"           . backward-down-list)
           ("M-r"             . raise-sexp)) ; was move-to-window-line-top-bottom

(use-package region-bindings-mode :demand
  :diminish 'region-bindings-mode
  :functions region-bindings-mode-enable
  :config
  (region-bindings-mode-enable))

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

(defun indent-list-or-sexp ()
  "Indent list at point, or the next sexp."
  (interactive)
  (let ((b (bounds-of-thing-at-point 'list))
        (p (point)))
    (if b (indent-region (car b) (cdr b))
      :else
      (save-excursion
        (forward-sexp)
        (indent-region p (point))))))
(bind-keys  ("C-M-\\" . indent-list-or-sexp))
(bind-keys :map region-bindings-mode-map
           ("C-M-\\" . indent-region))

(use-package shell
  :bind ("<f6>" . shell)
  :config
  (require 'pcmpl-args nil t))

(use-package z-misc
  :bind
  ("C-x ;" . z-align-char)
  ("C-x $" . z-toggle-selective-display)
  ("C-x /" . z-ediff-this-buffer)
  ("C-x _" . z-shrink-other-window-if-larger-than-buffer))

(use-package like-this
  :bind
  ("M-n" . like-this-next)
  ("M-p" . like-this-prev)
  :commands (like-this--next-face))

(defun z-kill-ring-save (arg)
  (interactive "P")
  "Forward to `kill-ring-save' or `clipetty-kill-ring-save' if ARG is non-nil
and we are on terminal."
  (if (and arg (frame-terminal))
      (call-interactively 'clipetty-kill-ring-save)
    (call-interactively 'kill-ring-save)))
(bind-keys :map region-bindings-mode-map
           ("M-w" . z-kill-ring-save))

(use-package clipetty
  :commands clipetty-kill-ring-save)

(use-package argatpt
  ;; generate autoloads
  :commands (forward-arg backward-arg))

(use-package easy-kill :ensure
  :functions (easy-kill-mark-region easy-kill-exit)
  :bind ("M-w" . easy-kill)
  :init
  (eval-when-compile
    (defmacro easy-kill-defun-on-selection (name func)
    `(easy-kill-defun ,name ()
       ,(concat "Call `" (symbol-name (cadr func)) "' on easy-kill selection.")
    (interactive)
    (save-mark-and-excursion
     (easy-kill-mark-region)
     (call-interactively ,func)))))
  :config

  (easy-kill-defun-on-selection easy-kill-transpose #'anchored-transpose)
  (easy-kill-defun-on-selection easy-kill-wrap-region #'self-insert-command)
  (easy-kill-defun-on-selection easy-kill-indent-region #'indent-region)
  (easy-kill-defun-on-selection easy-kill-comment-dwim #'comment-dwim)
  (easy-kill-defun-on-selection easy-kill-raise #'raise-sexp)

  (easy-kill-defun easy-kill-inside ()
    (interactive)
    (easy-pair-kill-inside (easy-kill-get start) (easy-kill-get end)))

  (defun easy-kill-delete-pairs ()
    (interactive)
    (easy-pair-delete (prefix-numeric-value current-prefix-arg)
                    (easy-kill-get start)
                    (easy-kill-get end))
    (when (eq (easy-kill-get start) (easy-kill-get end))
      (message "No selection, exit easy-kill")
      (easy-kill-exit)))

  (setq easy-kill-unhighlight-key (kbd "SPC"))
  (setq easy-kill-try-things '(url email arg sexp line))

  (setq easy-kill-alist '((?w word       " ")
                          (?s sexp       "\n")
                          (?l list       "\n")
                          (?f filename   "\n")
                          (?a arg        "\n")
                          (?d defun      "\n\n")
                          (?D defun-name " ")
                          (?p paragraph  "\n")
                          (?n line       "\n") ;changed from ?e
                          (?. sentence   " ")
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
   ("r"   . easy-kill-raise)
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

(use-package simple
  :diminish (auto-fill-function . " ¶")
  :diminish (visual-line-mode . " ↵")
  :diminish (next-error-follow-minor-mode . " ⇅")
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode . turn-on-auto-fill))
  :commands (cycle-spacing-0)
  :bind (("M-SPC"       . cycle-spacing)
         ("M-\\"        . cycle-spacing-0)
         ("M-c"         . capitalize-dwim)
         ("M-l"         . downcase-dwim)
         ("M-u"         . upcase-dwim)
         ("M-="         . z-toggle-activate-mark)
         ("<XF86Eject>" . keyboard-escape-quit)
         ([remap exchange-point-and-mark] . z-exchange-point-and-mark))
  :config
  (defun cycle-spacing-0 ()
    "Remove adjacent spaces, but undo if the command is issued a second time."
    (interactive)
    (cycle-spacing 0))

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

  (defun z-toggle-activate-mark ()
    "Toggle active region, without moving the mark."
    (interactive)
    (if (region-active-p)
        (deactivate-mark)
      (activate-mark)))

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
   (:else
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
   (:else
    (string-inflection-underscore-function str)))))

(use-package visual-regexp
  :bind ("C-M-%" . vr/query-replace))

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
             ("M-k"   . isearch-yank-word-or-char)
             ("M-z"   . isearch-yank-until-char)
             ("M-<"   . isearch-beginning-of-buffer)
             ("M->"   . isearch-end-of-buffer)
             ("C-j"   . avy-isearch))

  (setcdr (assq 'isearch-mode minor-mode-alist)
          '((:eval (if isearch-forward " »" " «")))))

(use-package find-file
  :bind ("C-x C-r" . ff-find-other-file))

(bind-keys ("C-x 9" . delete-other-windows-vertically)
           ("C-x C-0")                  ; unbind text-scale-adjust
           ("<mouse-8>" . mode-line-previous-buffer)
           ("<mouse-9>" . mode-line-next-buffer)
           ("C-z"))

;; F1 for help.
;; F3 and F4 for macros
(use-package gud
  :bind (("<f7>"   . gud-up)
         ("S-<f7>" . gud-down)
         ("<f8>"   . gud-next)
         ("S-<f8>" . gud-step)
         ("<f9>"   . gud-finish)))

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
(bind-keys ("<f10>"   . hydra-resize-window/body)
           ("<f11>"   . shrink-window)
           ("<f12>"   . enlarge-window)
           ("M-9"     . previous-buffer)
           ("M-0"     . next-buffer)
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
%s(ballotbox rainbow-delimiters-mode) rainbow-_d_elimiters  ^^ %s(ballotbox abbrev-mode \"∂\") _a_bbrev       %s(ballotbox outline-minor-mode) _o_utline-minor-mode ^^ %s(ballotbox beacon-mode) _b_eacon
%s(ballotbox rainbow-identifiers-mode) rainbow-_i_dentifiers ^^ %s(ballotbox auto-fill-function \"¶\") auto-_f_ill    %s(if (bound-and-true-p subword-mode) \",\" (if (bound-and-true-p superword-mode) \"²\" \"☐\")) sub_w_ord/super_W_ord   %s(ballotbox xterm-mouse-mode) _x_term-mouse
%s(ballotbox rainbow-mode) _R_ainbow colors       ^^%s(ballotbox visual-line-mode \"↵\") visual-lin_e_  %s(ballotbox flyspell-mode \"⍹\") fl_y_spell/_p_rog       %s(ballotbox electric-quote-mode) elec-_'_
%s(ballotbox hi-lock-mode) _h_i-lock/_c_hanges      %s(ballotbox auto-revert-mode \"↻\") auto-_r_evert  %s(ballotbox flycheck-mode \"✔\") flychec_k_            %s(ballotbox which-function-mode) which-f_u_nc
%s(ballotbox whitespace-mode \"␣\") white_s_pace/_t_railing  %s(ballotbox display-line-numbers-mode) line _n_um     %s(ballotbox flymake-mode) fly_m_ake
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
  ("w"    subword-mode)
  ("W"    superword-mode)
  ("x"    xterm-mouse-mode)
  ("y"    flyspell-mode)
  ("SPC"  nil)
  ("<f2>" nil)
)
(bind-key "<f2>" 'hydra-toggle/body)
(setq display-line-numbers-type 'relative)

(use-package subword
  :bind (("M-F" . subword-forward)
         ("M-B" . subword-backward)
         ("M-D" . subword-kill)
         ("M-T" . subword-transpose)))

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
  :commands (flyspell-mode flyspell-prog-mode)
  :hook (text-mode . turn-on-flyspell))

(use-package rainbow-delimiters :ensure
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package autorevert
  :diminish (auto-revert-mode . " ↻")
  :commands (auto-revert-mode))

(use-package whitespace :diminish " ␣"
  :commands (whitespace-mode))

(use-package abbrev
  :diminish (abbrev-mode . " ∂")
  :hook (text-mode . abbrev-mode))

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
          (:else (hs-toggle-hiding)))))

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

(use-package goto-chg :ensure
  :bind (("M-i" . goto-last-change)
         ("M-I" . goto-last-change-reverse)))

;; For use with avy
(defvar ctl-j-map (make-sparse-keymap)
  "Keymap behind C-j. Called by `z-goto-char'.")

(use-package avy :ensure :defer 5
  :bind ("C-j" . z-goto-char)
  :bind (:map minibuffer-local-map
              ("C-j" . z-goto-char))
  :bind (:map ctl-j-map
              ("SPC" . avy-goto-line)
              ("RET" . avy-show-dispatch)
              ("TAB" . avy-yank-word-1))
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

  (defun avy-yank-word-1 (char &optional arg beg end symbol)
    "Like `avy-goto-word-1', but yank instead."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-word-1
    (let* ((str (string char))
           (regex (cond ((string= str ".")
                         "\\.")
                        ((and avy-word-punc-regexp
                              (string-match avy-word-punc-regexp str))
                         (regexp-quote str))
                        ((<= char 26)
                         str)
                        (:else
                         (concat
                          (if symbol "\\_<" "\\b")
                          str)))))
      (avy-jump regex
                :window-flip arg
                :beg beg
                :end end
                :action 'avy-action-yank))))

  (defun avy-forward-char-in-line (char)
    "Jump to the currently visible CHAR in the current line after point."
    (interactive (list (read-char "char: " t)))
    (avy-with avy-goto-char
      (avy-jump (regexp-quote (string char))
                :beg (1+ (point))
                :end (line-end-position))))
  (defun avy-backward-char-in-line (char)
    "Jump to the currently visible CHAR in the current line before point."
    (interactive (list (read-char "char: " t)))
    (avy-with avy-goto-char
      (avy-jump (regexp-quote (string char))
                :beg (line-beginning-position)
                :end (point))))

  (defun z-goto-char (char &optional arg)
  "Call `avy-goto-char' or `avy-goto-subword-1', but respect bindings
in `ctl-j-map' first."
  (interactive (list (read-char "C-j ")
                     current-prefix-arg))
  (let ((act (lookup-key ctl-j-map (char-to-string char))))
    (cond (act (call-interactively act))
          ((string-match-p "[[:alpha:]]" (char-to-string char))
           (avy-goto-subword-1 char arg))
          (:else (avy-goto-char char arg)))))

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
  (push "*Placeholder*" aw-ignored-buffers)
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
  :bind ("M-g M-i" . ivy-imenu-anywhere)
  :config
  (defun imenu-anywhere-transformer (i)
    (save-match-data
      (string-match "^\\([^:]+\\): \\(\\w+/\\)?\\(.*\\)" i)
      (let* ((m (match-string 2 i))
             (c (if m (substring m 0 (1- (length m))) "Functions"))
             (rep (assoc c counsel-imenu-category-alist)))
        (if rep (concat (match-string 1 i) ": " (cdr rep) " " (match-string 3 i)) i))))
  (ivy-set-display-transformer 'ivy-imenu-anywhere 'imenu-anywhere-transformer))

(use-package recentf
  :config
  (setq recentf-max-saved-items 80)
  (setq recentf-exclude '("/elpa/")))

(use-package ivy :diminish ""
  :bind (("M-o" . ivy-switch-buffer)
         ("M-s M-d" . ivy-resume)
         ("M-g v" . ivy-pop-view)
         ("M-g M-v" . ivy-push-view))
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
         ("C-x C-d" . counsel-dired)
         ("C-x 8 8" . counsel-unicode-char)
         ("C-x b" . counsel-bookmark)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("M-s M-s" . counsel-grep-or-swiper)
         ("M-g m" . counsel-mark-ring)
         ("M-g i" . counsel-imenu)
         ("M-g r" . counsel-register))
  :bind (:map help-map
              ("S" . counsel-info-lookup-symbol)
              ("v" . counsel-describe-variable)
              ("f" . counsel-describe-function))
  :config
  ;; (setq counsel-describe-variable-function #'helpful-variable
  ;;       counsel-describe-function-function #'helpful-callable)

  (defun counsel-find-file-search ()
    "Switch to `counsel-file-jump', use current directory as base."
    (interactive)
    (let* ((input (ivy--input))
           (dir ivy--directory))
       (ivy-quit-and-run
         (counsel-file-jump input dir))))

  (defun counsel-find-file-dired ()
    "Switch to `counsel-dired-jump', use current directory as base."
    (interactive)
    (let* ((input (ivy--input))
           (dir ivy--directory))
       (ivy-quit-and-run
         (counsel-dired-jump input dir))))

  (bind-keys :map counsel-find-file-map
             ("M-s r" . counsel-find-file-search)
             ("C-x C-d" . counsel-find-file-dired))

  (setq counsel-find-file-ignore-regexp
        "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)\\|\\(\\`\\.\\)")

  (setq counsel-imenu-category-alist
        `(("Functions" . ,(propertize "ƒ" 'face 'font-lock-function-name-face))
          ("Packages"  . ,(propertize "℗" 'face 'font-lock-keyword-face))
          ("Sections"  . ,(propertize "§" 'face 'font-lock-constant-face))
          ("Types"     . ,(propertize "ᴛ" 'face 'font-lock-type-face))
          ("Variables" . ,(propertize "=" 'face 'font-lock-variable-name-face))))
  (defun counsel-imenu-transformer (i)
    (save-match-data
      (string-match "^\\(\\w+\\): \\(.*\\)" i)
      (let ((rep (assoc (match-string 1 i) counsel-imenu-category-alist)))
        (if rep (concat (cdr rep) " " (match-string 2 i)) i))))
  (ivy-set-display-transformer 'counsel-imenu 'counsel-imenu-transformer))

(use-package swiper
  :bind (("M-s s" . swiper-all))
  :bind  (:map isearch-mode-map
               ("M-s M-s" . swiper-isearch-toggle))
  :config
  (bind-keys :map swiper-map
             ("M-%" . swiper-query-replace)
             ("C-s" . swiper-isearch-toggle)
             ("C-j" . swiper-avy)))

(use-package xref
  :if (not (featurep 'google))
  :bind (("C-x ." . xref-find-definitions)
         ("C-x ?" . xref-find-references)
         ("C-x ," . xref-pop-marker-stack)))
