; -*- coding: utf-8; lexical-binding: t -*-

;;; Code:
(eval-when-compile
  (require 'use-package)
  (require 'hydra)
  (require 'keymap-hint))

;; Add several custom shortcuts in the TeX input method.
(eval-after-load "quail/Latin-ltx"
  '(load "init-tex-input-method"))

(defun z-setup-terminal ()
  ;; Translate ESC-* to M-*. This is needed for `read-key' to
  ;; recognize M-z in one step (otherwise it'll ready the ESC only).
  (cl-loop for c from ?! to ?~ do
           ;; M-[ and M-O are used in function keys
           (when (not (memq c '(?\[ ?O)))
             (keymap-set input-decode-map
               (concat "ESC " (char-to-string c))
               (concat "M-" (char-to-string c)))))
  (cl-loop for c from ?a to ?z do
    (keymap-set input-decode-map
      (concat "ESC C-" (char-to-string c))
      (concat "C-M-" (char-to-string c))))

  ;; CSI u style coding for C-. and C-M-. keys
  (cl-loop for c from ?! to ?~ do
           (let* ((s (format "%d" c))
                  (spaced (replace-regexp-in-string "\\([0-9]\\)" "\\1 " s)))
             (cl-loop for b in
                      '(("5" . "C-") ("7" . "C-M-")) do
                      (keymap-set input-decode-map
                        (concat "M-[ " spaced "; " (car b) " u")
                        (concat (cdr b) (char-to-string c)))))))
(add-hook 'tty-setup-hook #'z-setup-terminal)

(repeat-mode 1)

(use-package kmacro
  :config
  (setcdr (assq 'defining-kbd-macro minor-mode-alist)
          '((" "
             (:propertize
              (:eval (if (eq defining-kbd-macro 'append) "⊕" "●"))
              face (:foreground "#D04020")
              help-echo "Recording keyboard macro")))))

(keymap-hint-set ctl-x-r-map "?" "
Rectangle╶─────······───────╮ Register╶··┬╴save··╶─────╮ Bookmark──··─────╮
_c_lear     _N_umber-lines··│ _+_: inc   │ _␣_:point   │ _m_: set         │
_d_elete    _o_pen  s_t_ring│ _j_ump     │ _f_rameset  │ _b_: jump        │
_k_ill      _y_ank··        │ _i_nsert   │ _w_indow-cfg│ _l_ist           │
_M-w_:copy  _r_egister··    │ _s_ave text│ _n_umber    │ _M_: no-overwrite│
" :load)

(keymap-hint-set vc-prefix-map "?" "
_+_:update   ch_a_nge log    print _l_og   _d_ir     _h_istory     _m_erge
_=_:diff     log _I_ncoming  root _L_og    _P_ush    reg_i_ster    _r_etrieve tag
root-_D_iff  log _O_utgoing  _~_:revision  i_G_nore  _g_:annotate  _u_:revert
" :load)

(defun z-kill-buffer (arg)
  "Kill this buffer, or with ARG, call `kill-buffer' instead."
  (interactive "P")
  (if arg (call-interactively 'kill-buffer)
    (kill-buffer)))
(keymap-global-set "C-x k" 'z-kill-buffer)
(put 'z-kill-buffer 'command-semantic 'kill-buffer)

;; lisp is not a package
;; (use-package lisp)
(defun backward-down-list (arg)
  "Go down list backwards ARG times."
  (interactive "^p")
  (down-list (- (or arg 1))))

(bind-keys ("C-M-o" . up-list)
           ("C-M-j" . backward-down-list)
           ("C-M-l" . down-list)   ; was `reposition-window'
           ("M-r"   . raise-sexp)) ; was `move-to-window-line-top-bottom'


(defvar-local last-narrow-region nil
  "Remember last narrowing region, used by `narrow-dwim'.")
(defun narrow-dwim ()
  "If region is active, narrow to region. Otherwise toggle narrowing."
  (interactive)
  (cond ((use-region-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((buffer-narrowed-p)
         (setq last-narrow-region (cons (point-min-marker) (point-max-marker)))
         (widen))
        (last-narrow-region
         (narrow-to-region (car last-narrow-region) (cdr last-narrow-region))
         (setq last-narrow-region nil))
        ('t
         (message "No previous narrowing."))))
(bind-keys ("C-x n n" . narrow-dwim))


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

(use-package shell
  ;:bind ("<f6>" . shell)
  :config
  (require 'pcmpl-args nil t))

(use-package z-misc
  :bind
  ("M-."   . z-search-forward-char)
  ("M-,"   . z-search-backward-char)
  ("M-|"   . z-shell-command-on-buffer)
  ("C-x ;" . z-align-char)
  ("C-x $" . z-toggle-selective-display)
  ("C-x /" . z-ediff-this-buffer)
  ("C-x _" . z-shrink-other-window-if-larger-than-buffer)
  ("C-M-\\" . indent-list-or-sexp))
(bind-keys :map region-bindings-mode-map
           ("M-|"    . shell-command-on-region)
           ("C-M-\\" . indent-region))

(use-package like-this
  :bind
  ("M-n" . like-this-next)
  ("M-p" . like-this-prev))

(defun z-kill-ring-save (arg)
  "Wrapper around `kill-ring-save'.

If ARG is non-nil and we are on terminal, then call
`clipetty-kill-ring-save'."
  (interactive "P")
  (if (and arg (frame-terminal))
      (call-interactively 'clipetty-kill-ring-save)
    (call-interactively 'kill-ring-save)))
(keymap-set region-bindings-mode-map
           "M-w" #'z-kill-ring-save)

(use-package clipetty
  :commands clipetty-kill-ring-save)

(use-package argatpt
  :commands (transpose-args forward-arg backward-arg))

(use-package transpose-dwim
  :bind (("C-M-t" . transpose-dwim))
  :bind (:map region-bindings-mode-map
              ("C-t" . transpose-dwim-regions)))

(use-package easy-kill :ensure
  :functions (easy-kill-mark-region easy-kill-exit)
  :bind ([remap kill-ring-save] . easy-kill)
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

  (easy-kill-defun-on-selection easy-kill-transpose #'transpose-dwim-regions)
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
                          (?' string)
                          (?b buffer-file-name)))
  (bind-keys
   :map easy-kill-base-map
   ("<f1>" . easy-kill-help)
   ("C-h"  . easy-kill-help)
   ("DEL"  . easy-kill-delete-pairs)
   ("k"    . easy-kill-region)
   ("m"    . easy-kill-mark-region)
   ("i"    . easy-kill-inside)
   ("t"    . easy-kill-transpose)
   ("r"    . easy-kill-raise)
   (";"    . easy-kill-comment-dwim)
   ("("    . easy-kill-wrap-region)
   (")"    . easy-kill-wrap-region)
   ("["    . easy-kill-wrap-region)
   ("]"    . easy-kill-wrap-region)
   ("{"    . easy-kill-wrap-region)
   ("}"    . easy-kill-wrap-region)
   ("\""   . easy-kill-wrap-region)
   ("'"    . easy-kill-wrap-region)
   ("\\"   . easy-kill-indent-region)))

(use-package simple
  :diminish (auto-fill-function . " ¶")
  :diminish (visual-line-mode . " ↵")
  :diminish (next-error-follow-minor-mode . " ⇅")
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode . turn-on-auto-fill))
  :commands (cycle-spacing-0)
  :bind (("M-\\"        . cycle-spacing-0)
         ("M-c"         . capitalize-dwim)
         ("M-l"         . downcase-dwim)
         ("M-u"         . upcase-dwim)
         ("M-="         . z-toggle-activate-mark)
         ("C-?"         . undo-redo)
         ("<XF86Eject>" . keyboard-escape-quit)
         ("<f6>"        . scratch-buffer)
         ([remap exchange-point-and-mark] . z-exchange-point-and-mark))
  :config
  (setq next-error-message-highlight 't)

  (defun cycle-spacing-0 ()
    "Remove adjacent spaces, but undo if the command is issued a second time."
    (interactive)
    (let ((cycle-spacing-actions '(delete-all-space restore)))
      (call-interactively #'cycle-spacing)))

  ;; Decouple exchange-point-and-mark and activating region.
  (defun z-exchange-point-and-mark (&optional arg)
    "Like `exchange-point-and-mark', but ARG means toggle active region,
instead of inactivate region."
    (interactive "P")
    (let ((active (xor arg (use-region-p))))
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
             ("a" . move-beginning-of-line)
             ("e" . move-end-of-line)
             ("f" . forward-char)
             ("b" . backward-char)
             ("n" . next-line)
             ("p" . previous-line)
             ("{" . backward-paragraph)
             ("}" . forward-paragraph)
             ("[" . backward-page)
             ("]" . forward-page)
             ("c" . god-mode-self-insert)
             ("l" . god-mode-self-insert)
             ("v" . god-mode-self-insert)
             ("x" . god-mode-self-insert))
  (put 'previous-error-no-select 'command-semantic 'previous-line)
  (put 'next-error-no-select 'command-semantic 'next-line))

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


(keymap-hint-set resize-window-repeat-map "SPC" "
_^_ large _v_ shrink  _{_ _}_ horizontal
" :load)
(bind-keys ("<f10>"   . resize-window-repeat-map-hint)
           ("<f11>"   . shrink-window)
           ("<f12>"   . enlarge-window)
           ("M-9"     . previous-buffer)
           ("M-0"     . next-buffer)
           ("C-x 4 o" . display-buffer))
(put 'previous-buffer 'command-semantic 'switch-buffer)
(put 'next-buffer 'command-semantic 'switch-buffer)


(defun toggle-show-trailing-whitespace ()
   "Toggle `show-trailing-whitespace'."
   (interactive)
   (setq show-trailing-whitespace (not show-trailing-whitespace))
   (message "show-trailing-whitespace set to %s" show-trailing-whitespace))

;; Toggle commands

(defmacro ballotbox (var &optional pos)
  (if pos
  `(if (bound-and-true-p ,var) ,pos "·")
  `(if (bound-and-true-p ,var) "✔" "·")))

(defhydra hydra-toggle (:color blue :hint nil)
  "
Toggle:
%s(ballotbox rainbow-delimiters-mode) rainbow-_d_elimiters  ^^ %s(ballotbox abbrev-mode \"∂\") _a_bbrev       %s(ballotbox outline-minor-mode) _o_utline-minor-mode ^^ %s(ballotbox beacon-mode) _b_eacon
%s(ballotbox rainbow-identifiers-mode) rainbow-_i_dentifiers ^^ %s(ballotbox auto-fill-function \"¶\") auto-_f_ill    %s(if (bound-and-true-p subword-mode) \",\" (if (bound-and-true-p superword-mode) \"²\" \"·\")) sub_w_ord/super_W_ord   %s(ballotbox xterm-mouse-mode) _x_term-mouse
%s(ballotbox rainbow-mode) _R_ainbow colors       ^^%s(ballotbox visual-line-mode \"↵\") visual-lin_e_  %s(ballotbox flyspell-mode \"⍹\") fl_y_spell/_p_rog       %s(ballotbox electric-quote-mode) elec-_'_
%s(ballotbox hi-lock-mode) _h_i-lock/_c_hanges      %s(ballotbox auto-revert-mode \"↻\") auto-_r_evert  %s(ballotbox which-function-mode) which-f_u_nc
%s(ballotbox whitespace-mode \"␣\") white_s_pace/_t_railing  %s(ballotbox display-line-numbers-mode) line-_n_um     %s(ballotbox flymake-mode) fly_m_ake
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
(keymap-global-set "<f2>" 'hydra-toggle/body)
(setq display-line-numbers-type 'relative)

(use-package subword
  :bind (("M-F" . subword-forward)
         ("M-B" . subword-backward)
         ("M-D" . subword-kill)
         ("M-T" . subword-transpose)))

(use-package rainbow-mode :diminish rainbow-mode)

(use-package pulsar
  :init
  (setq pulsar-iterations 5)
  (pulsar-global-mode 1))

(use-package flyspell :diminish " ⍹"
  :commands (flyspell-mode flyspell-prog-mode)
  :hook (text-mode . turn-on-flyspell)
  :config
  (bind-keys :map flyspell-mode-map
            ("C-M-i")))

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
              ;;("C-k" . avy-kill-region)
              ("C-h" . ctl-j-help)
              ("TAB" . avy-yank-word-1))
  :config
  (defun ctl-j-help ()
    "Show help for ctl-j-map."
    (interactive)
    (describe-keymap ctl-j-map)
    (let ((inhibit-read-only t)
          (len (length "avy-action-"))
          (i 1))
      (with-current-buffer (help-buffer)
        (insert "Avy dispatch:\n")
        (mapc
         (lambda (x)
           (insert
            (format "%s: %s"
                    (propertize
                     (char-to-string (car x))
                     'face 'help-key-binding)
                    (substring (symbol-name (cdr x)) len)))
           (insert (if (eq (mod i 4) 0) "\n" " \t"))
           (setq i (1+ i)))
         avy-dispatch-alist)
        (unless (eq (mod i 4) 1)
          (insert "\n")))))

  ;; Used in avy, but defined in ace-window
  ;(put 'aw-key-face 'face-alias 'help-key-binding)

  (require 'subword)
  (setq avy-styles-alist '((avy-goto-char . de-bruijn))
        avy-keys
        '(?s ?d ?f ?g ?h ?j ?k ?l ?w ?e ?r ?u ?i ?o)
        avy-subword-extra-word-chars nil
        avy-orders-alist '((avy-goto-char . avy-order-closest)
                           (avy-goto-subword-1 . avy-order-closest)))

  (defun avy-action-on-word-1 (char &optional arg beg end symbol action)
    "Like `avy-goto-word-1', provide ACTION as argument."
    ;; Copy code of avy-goto-word-1. avy-with set avy-action to nil,
    ;; so we can not set a default action and just call avy-goto-word-1.
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
                  :action action))))

  (defun avy-yank-word-1 (char &optional arg beg end symbol)
    "Like `avy-goto-word-1', but yank instead."
    (interactive (list (read-char "char: " t)
                       current-prefix-arg))
    (avy-action-on-word-1 char arg beg end symbol 'avy-action-yank))

  (defun avy-goto-nth-char (char &optional n)
    "Jump to the currently visible N-th character in a sequence of CHAR.
N=9 is interpreted as the last one in sequence. N=0 is interpreted as passing the
last CHAR in a sequence."
    (interactive (list (read-char "char: " t)
                       current-prefix-arg))
    (let* ((regexp-char
            (if (= 13 char)
                "\n"
              (regexp-quote (string char))))
           regexp
           capture-group)
      (cond
       ((eq n 0)
        (setq regexp
              (concat regexp-char
                      "\\($\\|[^" (string char) "]\\)")
              capture-group 1))
       ((eq n 9)
        (setq regexp
              (concat regexp-char
                      "\\(?:$\\|[^" (string char) "]\\)")
              capture-group 0))
       ((numberp n)
        (setq regexp
              (concat "\\(?:^\\|[^"
                      (string char)
                      "]\\)" regexp-char "\\{" (number-to-string (1- n))
                      "\\}\\(" regexp-char "\\)")
              capture-group 1))
       (:else
        (setq regexp regexp-char
              capture-group 0)))
      (avy-with avy-goto-char
        ;;(setq avy-action (or action avy-action))
        (avy-process
         (avy--regex-candidates regexp nil nil nil capture-group)))))

  (defun z-goto-char (char &optional arg)
    "Call `avy-goto-nth-char' or `avy-goto-subword-1', but respect bindings
in `ctl-j-map' first."
    (interactive (list (read-char "C-j ")
                       current-prefix-arg))
    (let ((act (lookup-key ctl-j-map (char-to-string char))))
      (cond (act (call-interactively act))
            ((string-match-p "[[:alpha:]]" (char-to-string char))
             (avy-goto-subword-1 char arg))
            (:else (avy-goto-nth-char char arg)))))
)

(use-package window
  :bind* (("M-j" . other-window)
          ("M-J" . window-swap-states))
  :config
  (setq recenter-positions '(middle top bottom)))

(use-package zap-to-char-dwim
  :bind (("M-z" . zap-to-char-dwim)
         ("M-Z" . zap-back-to-char-dwim)))

(use-package find-dired
  :bind ("C-x d" . find-name-dired))

(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 80)
  (setq recentf-exclude '("/elpa/")))

(use-package xref
  :if (not (featurep 'google))
  :bind (("C-x ." . xref-find-definitions)
         ("C-x ?" . xref-find-references)
         ("C-x ," . xref-go-back)))

(use-package vertico :ensure
  :bind ("M-s M-d" . vertico-repeat)
  :init
  (vertico-mode)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  :config
  (setq vertico-resize t)
  (setq vertico-cycle t)
  (setq vertico-count-format nil))

(use-package orderless :ensure
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  (defun first-prefix-dispatch (pattern index _total)
    (if (= index 0) 'orderless-literal-prefix))
  (orderless-define-completion-style orderless-first-prefix
    (orderless-style-dispatchers '(first-prefix-dispatch
                                   orderless-affix-dispatch)))
  )

(use-package corfu
  :bind (:map corfu-map
              ([remap set-mark-command] . corfu-insert-separator))
  :init
  (global-corfu-mode))

(use-package marginalia :ensure
  :after vertico
  :init
  (marginalia-mode 1)
  :config
  (defun marginalia--buffer-status (buffer)
    "Return the status of BUFFER as a string."
    (format-mode-line '((:propertize
                         (:eval (z-buffer-status))
                         face marginalia-modified)
                        marginalia-separator
                        ;; InactiveMinibuffer has 18 letters, but there are longer names.
                        ;; For example Org-Agenda produces very long mode names.
                        ;; Therefore we have to truncate.
                        (20 (-20 (:propertize mode-name face marginalia-mode))))
                      nil nil buffer))

  ;; remove some less interesting info
  (defun marginalia--annotate-local-file (cand)
    "Annotate local file CAND."
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((file-size-human-readable (file-attribute-size attrs))
        :face 'marginalia-size :width -7)
       ((marginalia--time (file-attribute-modification-time attrs))
        :face 'marginalia-date :width -12))))

  (add-to-list 'marginalia-command-categories
               '(consult-find . file)))

(use-package consult :ensure
  :bind (("M-o" . consult-buffer)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ;; C-c C-l in `comint-mode-map'
         ([remap comint-dynamic-list-input-ring] . consult-history)
         ("C-x f" . consult-find)
         ("M-s M-s" . consult-line)
         ("M-s s" . consult-focus-lines)
         ("M-s M-g" . consult-grep)
         ("M-s M-r" . consult-ripgrep)
         ("M-s e" . consult-isearch-history)  ; similar to isearch-edit-string
         ;("M-y" . consult-yank-pop)
         ("M-g o" . consult-outline)
         ("M-g SPC" . consult-mark)
         ("M-g @" . consult-global-mark)
         ("M-g m" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("C-x C-z" . consult-complex-command))
  :config
  (setq consult-goto-line-numbers nil)

  ;; This might conflict with LSP stuff, see consult doc
  ;(setq completion-in-region-function #'consult-completion-in-region)
  (keymap-set consult-narrow-map "C-h" #'consult-narrow-help))

(use-package consult-imenu
  :bind (("M-g i" . consult-imenu)
         ("M-g M-i" . consult-imenu-multi)))

(use-package consult-compile
  :bind ("M-g `" . consult-compile-error))

(use-package consult-register
  :bind ("M-g r" . consult-register)
  :bind (:map ctl-j-map
              ("C-j" . consult-register-load)
              ("C-k" . consult-register-store))
  :init
  ;; Better preview of registers.
  (setq register-preview-function #'consult-register-format)
  ;; (advice-add #'register-preview :override #'consult-register-window)
  )

(use-package embark :ensure
  :after vertico
  :bind ("M-m" . embark-act)
  :bind (:map vertico-map
              ("M-s o" . embark-export)))

(use-package embark-consult :ensure
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-key)
;;; init-key.el ends here
