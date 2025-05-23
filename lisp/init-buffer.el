; -*- coding: utf-8; lexical-binding: t -*-

(eval-when-compile
  (require 'use-package)
  (require 'keymap-hint)
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

(keymap-hint-set ibuffer-mode-map "
_m_ark(_*_)╶╮ List······╶──╮ Buf Ops··╶─··─────────┬╴Togl╶····─╮ Text Ops····╶───····─────────╮
_%_:regexp··│ _s_ort _,_·· │ _D_elete  _j_ump      │_T_:∅ _M_:♦│ _Q_uery/r··     _!_:shell··  │
_t_oggle/_U_│ _/_filter····│ _S_ave    _v_iew      ╭╴Copy····╶─┤ _I_:q_r_-regex  _|_:pipe··   │
_d_el/_z_ap │ _g_:↻ _+__-_ │ re_V_ert  _o_ther-win │_B_name··  │ _=_:diff··      _N_:replace··│
_._old ··   │ _k_ill-ln····│ _R_ename  _H_:other-f │_w_:Fname··│ _O_ccur··       _E_val/vie_W_│
" :bind "SPC")

(put 'ibuffer-visit-buffer 'command-semantic 'switch-buffer)
(put 'ibuffer-do-view-other-frame 'command-semantic 'switch-frame)
(put 'ibuffer-do-occur 'command-semantic 'display-buffer)
(put 'ibuffer-visit-buffer-other-window 'command-semantic 'display-buffer)
(put 'ibuffer-do-view 'command-semantic 'switch-buffer)

(keymap-hint-set-sub ibuffer-mode-map "*" "
_*_ unmark all│ _M_ode _/_dired │ _u_nsaved    _m_odified      _z_:compressed
_c_hange      │ _h_elp _s_pecial│ _r_ead-only  _e_:dissociated
" :bind "SPC" :symbol ibuffer-mode-mark-map)


(keymap-hint-set-sub ibuffer-mode-map "%" "
_f_ilename  _m_ode  _n_ame  _g_rep  _L_ocked
" :bind "SPC")

(keymap-hint-set-sub ibuffer-mode-map "s" "
_a_lphabetic  _f_ilename  _m_ode  _s_ize  _v_:recency │ _i_nvert
" :bind "SPC")

(keymap-hint-set-sub ibuffer-mode-map "/" "
Filter by╶─(_/_ disable)····───────··──────╮ Ops╶······─────╮ Named╶··╮ _g_roups···╶────╮
_m_ode/derived_M_  _F_ dir      mod_i_fied │ Logic:_&__|__!_│ _s_ave  │ _S_ave/_R_evive·│
_b_ase/_n_ame      _._ ext      predicat_e_│ _t_:exchg····  │ _r_evive│ _X_:delete···   │
_f_ilename··       _*_ starred  _v_isiting │ _d_ecompose····│ _a_dd   │ _D_ecompose···  │
_<_ size _>_       _c_ontent    proc_E_ss  │ _↑_ _p_op··    │ _x_:del │ _P_op \\:clear··│
" :bind "SPC")

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
             ("M-o")                   ; ibuffer-visit-buffer-1-window
             ("M-j")
             ("e" . move-end-of-line) ; ibuffer-visit-buffer, also bound to f
             ("[" . ibuffer-backward-filter-group)
             ("]" . ibuffer-forward-filter-group)
             ("z" . ibuffer-do-kill-on-deletion-marks)
             ("x" . god-mode-self-insert))

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
  (add-hook 'ibuffer-hook 'z-ibuffer-hook)

  (put 'ibuffer-forward-line 'command-semantic 'next-line)
  (put 'ibuffer-backward-line 'command-semantic 'previous-line))

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
