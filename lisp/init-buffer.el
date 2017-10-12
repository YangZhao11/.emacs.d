; -*- coding: utf-8; lexical-binding: t -*-

(eval-when-compile
  (require 'use-package)
  (require 'hydra))

;; ==================================================
;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :functions ibuffer-switch-to-saved-filter-groups
  :config
  (defun g3-clients ()
    "Returns all g3 clients by iterating through all open files."
    (let* ((files (delq nil (mapcar 'buffer-file-name (buffer-list))))
           (f (cl-remove-if-not (lambda (x) (s-contains-p "/google3/" x)) files))
           (dirs (mapcar (lambda (x)
                           (replace-regexp-in-string
                            ".*/\\([^/]+\\)/google3/.*" "\\1" x)) f)))
      (cl-delete-duplicates dirs
                            :test (lambda (x y) (equal x y)))))

  (defvar z-default-g3-group
    '(("G3" (filename . "/googl3/")))
    "Default g3 group when g3 clients are not available,
e.g. no prodaccess.")

  (defun z-g3-group (d)
    "Function to map g3 client name to ibuffer filter group"
    (list (format "G3: %s" d)
          (cons 'filename
                (format "/%s/google3/" d))))

  (defun z-ibuffer-groups (name)
    "Generate ibuffer filter group definition. Each G3 client has its own group."
    (let ((g3 (g3-clients))
          (grps z-default-g3-group))
      (if g3
          (setq grps (mapcar 'z-g3-group g3)))
      (append (list name)
              grps
              `(("G3"
                 (filename . "/google3/"))
                (,(format "%s Home" (getenv "USER"))
                 ,(cons 'filename
                        (format "^%s/" (getenv "HOME"))))
                ("Misc" (or (mode . Custom-mode)
                            (mode . help-mode)
                            (mode . ess-help-mode)
                            (mode . apropos-mode)
                            (mode . completion-list-mode)
                            (name . "\\[r\\]$")
                            (name . "\\[fundamental\\]$")
                            (name . "^\\*Messages\\*$")
                            (name . "^\\*Backtrace\\*$")
                            (name . "^\\*Quail Completions\\*$")
                            (name . "^\\*.*Help.*\\*$")
                            (name . "^\\*[gP]4[- ]")
                            (name . "^\\*TeX ")
                            (name . "^\\*ESS\\*$")
                            (name . "^\\*ESS-")))))))

  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-eliding-string "…")
  (defadvice ibuffer-make-column-filename (after strip-google3-filename activate)
    "Strip experimental to ~user, .../a/google3 to //a/"
    (setq ad-return-value
          (replace-regexp-in-string "/google3/experimental/users/" "/google3/~"
                                    ad-return-value))
    (setq ad-return-value
          (replace-regexp-in-string "/google/src/cloud/.*/google3/" "//"
                                    ad-return-value))
    (setq ad-return-value
          (replace-regexp-in-string ".*/\\([^/]*\\)/google3/" "//\\1/"
                                    ad-return-value)))

  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark modified read-only " "
                (name 45 45 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide))))

  (defhydra hydra-ibuffer (:color pink :hint nil)
    "
_s_ort     _D_elete  _v_iew     ^^  _Q_uery      ‗Toggle^^‗  _F_:shell
_/_ filter _S_ave    _H_:other f^^  _r_eplace    _T_:RdOnly  _X_:pipe
_%_ regex  re_V_ert  _o_ther win^^  _I_:qr-regex _M_odified  _N_:replace
_*_ mark   _R_ename  vie_W_-_E_val  _O_ccur      _t_:mark    copy _B_name
"
    ("SPC" nil)
    ("RET" ibuffer-visit-buffer :exit t)
    ("*" hydra-ibuffer-mark/body :exit t)
    ("%" hydra-ibuffer-regex/body :exit t)
    ("/" hydra-ibuffer-filter/body :exit t)
    ("s" hydra-ibuffer-sort/body :exit t)
    ("B" ibuffer-copy-buffername-as-kill)
    ("D" ibuffer-do-delete)
    ("E" ibuffer-do-eval)
    ("F" ibuffer-do-shell-command-file)
    ("H" ibuffer-do-view-other-frame :exit t)
    ("I" ibuffer-do-query-replace-regexp)
    ("N" ibuffer-do-shell-command-pipe-replace)
    ("M" ibuffer-do-toggle-modified)
    ("o" ibuffer-visit-buffer-other-window :exit t)
    ("O" ibuffer-do-occur)
    ("Q" ibuffer-do-query-replace)
    ("q" quit-window :exit t)
    ("r" ibuffer-do-replace-regexp)
    ("R" ibuffer-do-rename-uniquely)
    ("S" ibuffer-do-save)
    ("T" ibuffer-do-toggle-read-only)
    ("t" ibuffer-toggle-marks)
    ("v" ibuffer-do-view :exit t)
    ("V" ibuffer-do-revert)
    ("W" ibuffer-do-view-and-eval)
    ("X" ibuffer-do-shell-command-pipe)
    ("x" ibuffer-do-kill-on-deletion-marks))

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
Filter by  (_DEL_ disable)‗‗‗‗‗‗‗‗‗‗‗^^^^^^  Op‗‗‗‗‗‗‗^^^^^^   _g_roups‗‗‗‗‗‗
_m_ode/derived_M_  _/_ dir      mod_i_fied   _!_ _&_ _|_       _S_ave/_R_evive
_b_ase/_n_ame      _._ ext      predicat_e_  _t_:exchg^^^^     _X_:delete
_f_ilename^^       _*_ starred  _v_isiting   _d_ecompose^^^^   _D_ecompose
_<_ size _>_       _c_ontent    ^^           _<up>_ pop^^^^    _P_op \\:clear
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
    ("f" ibuffer-filter-by-filename)
    ("g" ibuffer-filter-to-filter-group)
    ("i" ibuffer-filter-by-modified)
    ("v" ibuffer-filter-by-visiting-file)
    (">" ibuffer-filter-by-size-gt)
    ("<" ibuffer-filter-by-size-lt)
    ("D" ibuffer-decompose-filter-group)
    ("P" ibuffer-pop-filter-group)
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
    )
  (bind-keys :map ibuffer-mode-map
             ("M-o" . nil)             ; ibuffer-visit-buffer-1-window
             ("[" . ibuffer-backward-filter-group)
             ("]" . ibuffer-forward-filter-group)
             ("/ /" . ibuffer-filter-by-directory) ;somehow not default
             ("/ DEL" . ibuffer-filter-disable)    ;somehow not default
             ("SPC" . hydra-ibuffer/body))

  (defun z-ibuffer-mode-hook ()
    (ibuffer-switch-to-saved-filter-groups "Default"))
  (add-hook 'ibuffer-mode-hook 'z-ibuffer-mode-hook)
  (defun z-ibuffer-hook ()
    (setq ibuffer-saved-filter-groups (list (z-ibuffer-groups "Default"))))
  (z-ibuffer-hook)             ; ensure groups are set on the first run.
  (add-hook 'ibuffer-hook 'z-ibuffer-hook))

;; how to solve collision of buffer names: filename:pathpart
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator ":"))

;; midnight mode cleans up unused buffer
(use-package midnight
  :init
  (midnight-delay-set 'midnight-delay "4:30am")
  (add-to-list 'clean-buffer-list-kill-regexps "\\*help\\[R\\]")
  (add-to-list 'clean-buffer-list-kill-regexps "\\*[Pg]4[- ]"))

;; always need a scratch buffer.
(defun z-idle-timer () (get-buffer-create "*scratch*"))
(run-with-idle-timer 1 t 'z-idle-timer)
