; -*- coding: utf-8; lexical-binding: t -*-

(eval-when-compile
  (require 'use-package)
  (require 'hydra)
  (require 'subr-x))

;; manually configured project type that matches given patterns
(require 'project)
(setq project-manual-root-pattern
      (list
       "~/Projects/[^/]*/"
       (replace-regexp-in-string "/lisp/.*" "/"
                                 ;; emacs lisp files
                                 (locate-library "simple"))))

(defun project-try-manual (dir)
  (let* ((dir-normalized (abbreviate-file-name dir))
         (root (save-match-data
                (seq-find 'identity
                          (seq-map
                           (lambda (root-pattern)
                             (if (string-match root-pattern dir-normalized)
                                 (match-string 0 dir-normalized)))
                           project-manual-root-pattern)))))
    (if root (cons 'manual root))))

(cl-defmethod project-root ((project (head manual)))
  (cdr project))

(setq project-find-functions '(project-try-vc project-try-manual))
(if (require 'project-g3 nil t)
    (add-to-list 'project-find-functions 'project-try-g3))

(use-package ibuffer-project
  :config
  (setq ibuffer-project-use-cache 't))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :functions ibuffer-switch-to-saved-filter-groups
  :config

  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-eliding-string "…"
        ibuffer-expert t)

  (defvar long-filename-short-replacements
        '(("/google3/experimental/users/" "/google3/~")
          ("/google/src/cloud/.*/google3/" "//")
          (".*/\\([^/]*\\)/google3/" "//\\1/"))
        "Replacement regexps to shorten filename in ibuffer.")
  (defun long-filename-short (fn)
    "Strip several long format names"
    (dolist (rep long-filename-short-replacements)
      (setq fn
            (replace-regexp-in-string (car rep) (cadr rep) fn)))
    fn)
  (advice-add 'ibuffer-make-column-filename
              :filter-return #'long-filename-short)

  (define-ibuffer-column buffer-status
    (:name "*" :inline t)
    (z-buffer-status))

  (setq ibuffer-formats
        '((mark buffer-status " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark buffer-status " "
                (name 45 45 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide))))

  (defhydra hydra-ibuffer (:color pink :hint nil)
    "
_m_ark(_*_)╶╮ List^^^^^^╶──╮ Buf Ops^^╶─^^─────────┬╴Togl╶^^^^─╮ Text Ops^^^^╶───^^^^─────────╮
_%_:regexp^^│ _s_ort _,_^^ │ _D_elete  _j_ump      │_T_:∅ _M_:♦│ _Q_uery/r^^     _!_:shell^^  │
_t_oggle/_U_│ _/_filter^^^^│ _S_ave    _v_iew      ╭╴Copy^^^^╶─┤ _I_:q_r_-regex  _|_:pipe^^   │
_d_el/_z_ap │ _g_:↻ _+__-_ │ re_V_ert  _o_ther-win │_B_name^^  │ _=_:diff^^      _N_:replace^^│
_._old ^^   │ _k_ill-ln^^^^│ _R_ename  _H_:other-f │_w_:Fname^^│ _O_ccur^^       _E_val/vie_W_│
"
    ("SPC" nil)
    ("RET" ibuffer-visit-buffer :exit t)
    ("%" hydra-ibuffer-regex/body :exit t)
    ("*" hydra-ibuffer-mark/body :exit t)
    ("/" hydra-ibuffer-filter/body :exit t)
    ("-" ibuffer-add-to-tmp-hide)
    ("+" ibuffer-add-to-tmp-show)
    ("=" ibuffer-diff-with-file)
    ("," ibuffer-toggle-sorting-mode)
    ("." ibuffer-mark-old-buffers)
    ("B" ibuffer-copy-buffername-as-kill)
    ("w" ibuffer-copy-filename-as-kill)
    ("D" ibuffer-do-delete)
    ("E" ibuffer-do-eval)
    ("!" ibuffer-do-shell-command-file)
    ("H" ibuffer-do-view-other-frame :exit t)
    ("I" ibuffer-do-query-replace-regexp)
    ("M" ibuffer-do-toggle-modified)
    ("N" ibuffer-do-shell-command-pipe-replace)
    ("O" ibuffer-do-occur)
    ("Q" ibuffer-do-query-replace)
    ("R" ibuffer-do-rename-uniquely)
    ("S" ibuffer-do-save)
    ("T" ibuffer-do-toggle-read-only)
    ("V" ibuffer-do-revert)
    ("U" ibuffer-unmark-all-marks)
    ("W" ibuffer-do-view-and-eval)
    ("|" ibuffer-do-shell-command-pipe)
    ("d" ibuffer-mark-for-delete)
    ("g" ibuffer-update)
    ("j" ibuffer-jump-to-buffer)
    ("k" ibuffer-do-kill-lines)
    ("m" ibuffer-mark-forward)
    ("o" ibuffer-visit-buffer-other-window :exit t)
    ("q" quit-window :exit t)
    ("r" ibuffer-do-replace-regexp)
    ("s" hydra-ibuffer-sort/body :exit t)
    ("t" ibuffer-toggle-marks)
    ("u" ibuffer-unmark-forward)
    ("v" ibuffer-do-view :exit t)
    ("z" ibuffer-do-kill-on-deletion-marks))

  (defhydra hydra-ibuffer-mark (:color teal :columns 5 :hint nil
                                       :after-exit
                                       (if (eq major-mode 'ibuffer-mode)
                                           (hydra-ibuffer/body)))
    "Mark"
    ("SPC" nil)
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("c" ibuffer-change-marks "change")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("%" hydra-ibuffer-regex/body "regex"))

  (defhydra hydra-ibuffer-regex (:color teal :hint nil
                                        :after-exit
                                        (if (eq major-mode 'ibuffer-mode)
                                            (hydra-ibuffer/body)))
    "Regex"
    ("SPC" nil)
    ("*" hydra-ibuffer-mark/body "mark")
    ("f" ibuffer-mark-by-file-name-regexp "filename")
    ("m" ibuffer-mark-by-mode-regexp "mode")
    ("n" ibuffer-mark-by-name-regexp "name")
    ("g" ibuffer-mark-by-content-regexp "grep")
    ("L" ibuffer-mark-by-locked "locked") ; maybe default binding will change
    )

  (defhydra hydra-ibuffer-sort (:color teal :columns 3 :hint nil
                                       :after-exit
                                       (if (eq major-mode 'ibuffer-mode)
                                           (hydra-ibuffer/body)))
    "Sort"
    ("SPC" nil)
    ("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode"))

  (defhydra hydra-ibuffer-filter (:color teal :hint nil
                                         :after-exit
                                         (if (eq major-mode 'ibuffer-mode)
                                             (hydra-ibuffer/body)))
    "
Filter by╶─(^^⌫ disable)^^^^──╮ Ops^^^^^^^^╶────────────────╮ _g_roups^^╶────╮
_m_ode/derived_M_  _/_ dir    │ mod_i_fied   Logic:_&__|__!_│ _S_ave/_R_evive│
_b_ase/_n_ame      _._ ext    │ predicat_e_  _t_:exchg^^^^  │ _X_:delete^^   │
_f_ilename^^       _*_ starred│ _v_isiting   _d_ecompose^^^^│ _D_ecompose^^  │
_<_ size _>_       _c_ontent  │ proc_E_ss    _↑_ _p_op^^    │ _P_op \\:clear^│
"
    ("SPC" nil)
    ("DEL" ibuffer-filter-disable)
    ("m" ibuffer-filter-by-used-mode)
    ("M" ibuffer-filter-by-derived-mode)
    ("n" ibuffer-filter-by-name)
    ("b" ibuffer-filter-by-basename)
    ("." ibuffer-filter-by-file-extension)
    ("/" ibuffer-filter-by-directory)
    ("*" ibuffer-filter-by-starred-name)
    ("c" ibuffer-filter-by-content)
    ("e" ibuffer-filter-by-predicate)
    ("E" ibuffer-filter-by-process)
    ("f" ibuffer-filter-by-filename)
    ("g" ibuffer-filters-to-filter-group)
    ("i" ibuffer-filter-by-modified)
    ("v" ibuffer-filter-by-visiting-file)
    (">" ibuffer-filter-by-size-gt)
    ("<" ibuffer-filter-by-size-lt)
    ("D" ibuffer-decompose-filter-group)
    ("P" ibuffer-pop-filter-group)
    ("p" ibuffer-pop-filter)
    ("R" ibuffer-switch-to-saved-filter-groups)
    ("S" ibuffer-save-filter-groups)
    ("X" ibuffer-delete-saved-filter-groups)
    ("\\" ibuffer-clear-filter-groups)
    ("!" ibuffer-negate-filter)
    ("&" ibuffer-and-filter)
    ("a" ibuffer-add-saved-filters)
    ("d" ibuffer-decompose-filter)
    ("t" ibuffer-exchange-filters)
    ("|" ibuffer-or-filter)
    ("x" ibuffer-delete-saved-filters)
    ("<up>" ibuffer-pop-filter)
    ("↑" undefined)
    )

  (defun ibuffer-dired-jump (&optional other-window)
    "jump to current file in dired"
    (interactive)
    (let* ((buf (ibuffer-current-buffer))
           (file-name (buffer-file-name buf)))
      (if file-name
          (dired-jump other-window file-name)
        (let ((dir (buffer-local-value 'default-directory buf)))
          (if other-window
              (dired-other-window dir)
            (dired dir))))))
  (defun ibuffer-dired-jump-other-window ()
    "jump to current file in dired in other window"
    (interactive)
    (ibuffer-dired-jump 't))
  (bind-keys :map ibuffer-mode-map
             ("C-x C-j" . ibuffer-dired-jump)
             ("C-x 4 j" . ibuffer-dired-jump-other-window)
             ("M-o")             ; ibuffer-visit-buffer-1-window
             ("M-j")
             ("[" . ibuffer-backward-filter-group)
             ("]" . ibuffer-forward-filter-group)
             ("z" . ibuffer-do-kill-on-deletion-marks)
             ("x" . god-mode-self-insert)
             ("SPC" . hydra-ibuffer/body))

  (defun z-ibuffer-mode-hook ()
    ;;(ibuffer-switch-to-saved-filter-groups "Default")
    (setq-local page-delimiter "^\\[ \\(.*\\)? \\]$"))
  (add-hook 'ibuffer-mode-hook 'z-ibuffer-mode-hook)
  (defun z-ibuffer-hook ()
    (setq ibuffer-filter-groups
          `(("Misc" (or (mode . Custom-mode)
                        (mode . help-mode)
                        (mode . ess-help-mode)
                        (mode . apropos-mode)
                        (mode . completion-list-mode)
                        (mode . magit-diff-mode)
                        (mode . magit-log-mode)
                        (mode . magit-process-mode)
                        (name . "^\\*Async-native-compile-log\\*$")
                        (name . "\\[r\\]$")
                        (name . "\\[fundamental\\]$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Warnings\\*$")
                        (name . "^\\*Backtrace\\*$")
                        (name . "^\\*Quail Completions\\*$")
                        (name . "^\\*.*Help.*\\*$")
                        (name . "^\\*[gP]4[- ]")
                        (name . "^\\*TeX ")
                        (name . "^\\*ESS\\*$")
                        (name . "^\\*ESS-")))
            . ,(ibuffer-project-generate-filter-groups))))
  (z-ibuffer-hook)             ; ensure groups are set on the first run.
  (add-hook 'ibuffer-hook 'z-ibuffer-hook))

;; how to solve collision of buffer names: filename:pathpart
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward))

;; midnight mode cleans up unused buffer
(use-package midnight
  :init
  (midnight-delay-set 'midnight-delay "4:30am")
  (add-to-list 'clean-buffer-list-kill-regexps "\\*help\\[R\\]")
  (add-to-list 'clean-buffer-list-kill-regexps "\\*[Pg]4[- ]"))

;; always need a scratch buffer.
(defun z-idle-timer () (get-buffer-create "*scratch*"))
(run-with-idle-timer 1 t 'z-idle-timer)

(provide 'init-buffer)
