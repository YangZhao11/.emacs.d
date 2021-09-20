; -*- coding: utf-8; lexical-binding: t -*-

(eval-when-compile
  (require 'use-package)
  (require 'hydra))

(use-package bookmark
  :config
  (defhydra hydra-bookmark-bmenu (:color pink :hint nil)
    "
_m_ark     _u_nmark    _1_ window  _v_isit    _s_ave/_l_oad    _a_nnotation
_d_elete   _DEL_: back _2_ window  _r_ename   _t_oggle fname^^ _A_ll
_z_ap      _RET_: go   _o_ther win _R_elocate _w_here^^        _e_dit
"
    ("SPC" nil)
    ("m" bookmark-bmenu-mark)
    ("v" bookmark-bmenu-select :color blue)
    ("t" bookmark-bmenu-toggle-filenames)
    ("w" bookmark-bmenu-locate)
    ("1" bookmark-bmenu-1-window :color blue)
    ("2" bookmark-bmenu-2-window :color blue)
    ("RET" bookmark-bmenu-this-window :color blue)
    ("o" bookmark-bmenu-other-window :color blue)
    ("r" bookmark-bmenu-rename)
    ("R" bookmark-bmenu-relocate)
    ("d" bookmark-bmenu-delete)
    ("z" bookmark-bmenu-execute-deletions)
    ("s" bookmark-bmenu-save)
    ("l" bookmark-bmenu-load)
    ("u" bookmark-bmenu-unmark)
    ("q" quit-window :exit t)
    ("DEL" bookmark-bmenu-backup-unmark)
    ("a" bookmark-bmenu-show-annotation)
    ("A" bookmark-bmenu-show-all-annotations)
    ("e" bookmark-bmenu-edit-annotation))
  (bind-keys :map bookmark-bmenu-mode-map
             ("SPC" . hydra-bookmark-bmenu/body)
             ("z" . bookmark-bmenu-execute-deletions)
             ("x" . god-mode-self-insert))
)

(use-package view :diminish view-mode
  :bind ("C-x C-v" . view-mode)         ; find-alternate-file
  :config
  (defhydra hydra-view (:color pink :hint nil)
    "
^^pg/set^^ line^^ half^^ _g_o/_%_ _{__}_ parag ^^register  ^^mark   _s_earch/_r_    _q_uit/_Q_
_k_↑ _K_   _p_    _u_p   _<_^^    _[__]_ page  _m_ark      _._set   regex: _/_ ^\\  ^_i_menu
_j_↓ _J_   _n_    _d_own _>_^^    _(__)_ list  _'_: goto   p_@_p    again: _S_ _R_  ^_o_utline
"
    ("SPC" nil)
    ("j" View-scroll-page-forward)
    ("k" View-scroll-page-backward)
    ("J" View-scroll-page-forward-set-page-size)
    ("K" View-scroll-page-backward-set-page-size)
    ("n" View-scroll-line-forward)
    ("p" View-scroll-line-backward)
    ("u" View-scroll-half-page-backward)
    ("d" View-scroll-half-page-forward)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("g" View-goto-line)
    ("%" View-goto-percent)
    ("s" isearch-forward)
    ("r" isearch-backward)
    ("\\" View-search-regexp-backward)
    ("/" View-search-regexp-forward)
    ("S" View-search-last-regexp-forward)
    ("R" View-search-last-regexp-backward)
    ("m" point-to-register)
    ("'" register-to-point)
    ("." set-mark-command)
    ("@" View-back-to-mark)
    ("{" backward-paragraph)
    ("}" forward-paragraph)
    ("[" backward-page)
    ("]" forward-page)
    ("(" backward-list)
    (")" forward-list)
    ("q" View-leave :color blue)
    ("Q" View-quit :color blue)
    ("C" View-kill-and-leave :color blue)
    ("i" consult-imenu)
    ("o" consult-outline))
  (bind-keys :map view-mode-map
             ("SPC" . hydra-view/body)
             ("C-j" . nil)
             ("i" . consult-imenu)
             ("o" . consult-outline)
             ("x" . god-mode-self-insert)
             ("c" . god-mode-self-insert)
             ("l" . recenter-top-bottom)
             ("{" . backward-paragraph)
             ("}" . forward-paragraph)
             ("[" . backward-page)
             ("]" . forward-page)
             ("(" . backward-list)
             (")" . forward-list)
             ("q" . View-leave)
             ("Q" . View-quit)
             ("z" . repeat)
             ("n" . View-scroll-line-forward)
             ("p" . View-scroll-line-backward)
             ("S" . View-search-last-regexp-forward)
             ("R" . View-search-last-regexp-backward)
             ("j" . View-scroll-page-forward)
             ("k" . View-scroll-page-backward)
             ("J" . View-scroll-page-forward-set-page-size)
             ("K" . View-scroll-page-backward-set-page-size))
  (add-hook 'view-mode-hook
            (lambda () (if view-mode (god-local-mode-pause)
                    (god-local-mode-resume)))))

(use-package replace
  :bind ("M-s M-o" . multi-occur-in-matching-buffers)
  :config
  (defhydra hydra-occur (:color pink :hint nil)
    "
_k_↑   _p_rev^^   _<_ _>_       _RET_: goto      _e_dit
_j_↓   _n_ext^^   _d_isplay^^   _o_ther window   %s(if next-error-follow-minor-mode \"⇅\" \"☐\") _f_ollow
"
    ("SPC" nil)
    ("j" scroll-up-command)
    ("k" scroll-down-command)
    ("p" occur-prev)
    ("n" occur-next)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("d" occur-mode-display-occurrence)
    ("e" occur-edit-mode :exit t)
    ("q" quit-window :exit t)
    ("f" next-error-follow-minor-mode)
    ("o" occur-mode-goto-occurrence-other-window :exit t)
    ("RET" occur-mode-goto-occurrence :exit t))
  (bind-keys :map occur-mode-map
             ("d" . occur-mode-display-occurrence)
             ("j" . scroll-up-command)
             ("k" . scroll-down-command)
             ("n" . occur-next)
             ("p" . occur-prev)
             ("x" . god-mode-self-insert)
             ("c" . god-mode-self-insert)
             ("SPC" . hydra-occur/body)))

(use-package grep
  :bind (("M-s g"   . grep)
         ("M-s M-g" . rgrep))
  :config
  (defhydra hydra-grep (:color pink :hint nil)
  "
_k_↑^^  _p_rev^^  _<__>_  beg/end of buffer _RET_: goto  _e_dit
_j_↓^^  _n_ext^^  _{__}_: prev/next file    _d_isplay
"
  ("SPC" nil)
  ("p" previous-error-no-select)
  ("n" next-error-no-select)
  ("j" compilation-next-error)
  ("k" compilation-previous-error)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("{" compilation-previous-file)
  ("}" compilation-next-file)
  ("d" compilation-display-error)
  ("e" wgrep-change-to-wgrep-mode :exit t)
  ("q" quit-window :exit t)
  ("f" next-error-follow-minor-mode)
  ("RET" compile-goto-error :exit t))
  (bind-keys :map grep-mode-map
             ("SPC" . hydra-grep/body)
             ("j" . compilation-next-error)
             ("k" . compilation-previous-error)
             ("e" . wgrep-change-to-wgrep-mode)
             ("d" . compilation-display-error)
             ("x" . god-mode-self-insert)
             ("c" . god-mode-self-insert)))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode)

(use-package compile
  :config
  (defhydra hydra-compilation (:color pink :hint nil)
    "
_k_↑  _p_rev  _<__>_  beg/end of buffer _RET_: goto
_j_↓  _n_ext  _{__}_: prev/next file
"
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("g" recompile)
    ("q" quit-window :color blue)
    ("j" scroll-up-command)
    ("k" scroll-down-command)
    ("n" compilation-next-error)
    ("p" compilation-previous-error)
    ("{" compilation-previous-file)
    ("}" compilation-next-file)
    ("RET" compile-goto-error)
    ("SPC" nil))
  (bind-keys :map compilation-mode-map
             ("SPC" . hydra-compilation/body)
             ("x" . god-mode-self-insert)
             ("c" . god-mode-self-insert)
             ("`" . next-error)
             ("j" . scroll-up-command)
             ("k" . scroll-down-command)
             ("n" . compilation-next-error)
             ("p" . compilation-previous-error)
             ("{" . compilation-previous-file)
             ("}" . compilation-next-file)))

(use-package ibuffer-sidebar
  :bind ("M-s b" . z-ibuffer-sidebar)
  :config
  (setq ibuffer-sidebar-width 30)
  (defun z-ibuffer-sidebar ()
    (interactive)
    (imenu-list-minor-mode -1)
    (ibuffer-sidebar-toggle-sidebar)))

(use-package dired-sidebar
  :bind ("M-s j" . z-dired-sidebar)
  :config
  (setq dired-sidebar-theme 'nerd
        dired-sidebar-width 30)
  (defun z-dired-sidebar ()
    (interactive)
    (imenu-list-minor-mode -1)
    (dired-sidebar-toggle-sidebar)))

(use-package dired
  :bind (("C-x C-d" . dired)
         ("C-x C-j" . dired-jump)
         ("C-x 4 j" . dired-jump-other-window))
  :config
  (require 'dired-x)
  (setq dired-dwim-target 't)

  (defhydra hydra-dired (:color pink :columns 3 :hint nil)
    "
^Mark^‗‗‗‗‗‗‗^Flag^‗‗‗‗‗‗‗‗^Emacs Op^‗‗‗‗‗‗^‗^‗‗‗‗‗‗‗‗‗‗‗‗‗^^File Op^^‗‗(_e_dit)
_*_: marks^^   _#_: temp^^     _Q_uery/rep     _F_ind marked   _!_shell_&_ _S_ymlink
_%_: regexp^^  _~_: backup^^   _A_: grep       _L_oad          ^^_C_opy    _H_ardlink
_u_n/_m_ark    _d_: this^^     _B_yte compile  _k_ill line     ^^_D_elete  ch_M_od
_t_oggle/_U_   _z_ap^^         _v_iew          _w_: file name  ^^_R_ename  ch_O_wn
_[_ _]_:page   _<_ _>_:dirline _o_ther win     redisp_l_ay     ^^_T_ouch   ch_G_rp
"
    ("SPC" nil)
    ("RET" dired-find-file :exit t)
    ("q" quit-window :exit t)
    ("e" dired-toggle-read-only)
    ("!" dired-do-shell-command)
    ("m" dired-mark)
    ("u" dired-unmark)
    ("#" dired-flag-auto-save-files)
    ("$" dired-hide-subdir "hide subdir")
    ("%" hydra-dired-regexp/body :exit t)
    ("&" dired-do-async-shell-command)
    ("(" dired-hide-details-mode "hide details")
    ("*" hydra-dired-mark/body :exit t)
    ("+" dired-create-directory "create dir")
    ("." dired-clean-directory "clean dir")
    ("<" dired-prev-dirline)
    ("=" dired-diff "diff")
    (">" dired-next-dirline)
    ("[" backward-page)
    ("]" forward-page)
    ("A" dired-do-find-regexp)
    ("B" dired-do-byte-compile)
    ("C" dired-do-copy)
    ("D" dired-do-delete)
    ("F" dired-do-find-marked-files :exit t)
    ("G" dired-do-chgrp)
    ("H" dired-do-hardlink)
    ("L" dired-do-load)
    ("M" dired-do-chmod)
    ("O" dired-do-chown)
    ("P" dired-do-print "print")
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("S" dired-do-symlink)
    ("T" dired-do-touch)
    ("U" dired-unmark-all-marks)
    ("W" browse-url-of-dired-file "Web")
    ("Z" dired-do-compress "compress")
    ("^" dired-up-directory "up-directory")
    ("a" dired-find-alternate-file "find-alternate-file")
    ("c" dired-do-compress-to "compress-to")
    ("d" dired-flag-file-deletion)
    ("i" dired-maybe-insert-subdir "maybe-insert-subdir")
    ("j" dired-goto-file "goto-file")
    ("k" dired-do-kill-lines)
    ("l" dired-do-redisplay)
    ("o" dired-find-file-other-window :exit t)
    ("s" dired-sort-toggle-or-edit "sort-toggle-or-edit")
    ("t" dired-toggle-marks)
    ("v" dired-view-file :exit t)
    ("w" dired-copy-filename-as-kill)
    ("z" dired-do-flagged-delete)
    ("y" dired-show-file-type "show-file-type")
    ("~" dired-flag-backup-files))

  (defhydra hydra-dired-mark (:color teal :columns 3 :hint nil
                              :after-exit
                                (if (eq major-mode 'dired-mode)
                                    (hydra-dired/body)))
    "Mark"
    ("SPC" nil)
    ("!" dired-unmark-all-marks  "unmark all")
    ("%" dired-mark-files-regexp "regexp")
    ("(" dired-mark-sexp         "sexp")
    ("*" dired-mark-executables  "executables")
    ("." dired-mark-extension    "extension")
    ("/" dired-mark-directories  "directories")
    ("?" dired-unmark-all-files  "unmark markchar")
    ("@" dired-mark-symlinks     "symlinks")
    ("O" dired-mark-omitted      "omitted")
    ("c" dired-change-marks      "change")
    ("s" dired-mark-subdir-files "subdir-files"))

  (defhydra hydra-dired-regexp (:color teal :columns 3 :hint nil
                                :after-exit
                                (if (eq major-mode 'dired-mode)
                                    (hydra-dired/body)))
    "Regexp"
    ("SPC" nil)
    ("&" dired-flag-garbage-files "flag-garbage-files")
    ("C" dired-do-copy-regexp "copy")
    ("H" dired-do-hardlink-regexp "hardlink")
    ("R" dired-do-rename-regexp "rename")
    ("S" dired-do-symlink-regexp "symlink")
    ("Y" dired-do-relsymlink-regexp "relsymlink")
    ("d" dired-flag-files-regexp "flag-files")
    ("g" dired-mark-files-containing-regexp "mark-containing")
    ("l" dired-downcase "downcase")
    ("m" dired-mark-files-regexp "mark")
    ("r" dired-do-rename-regexp "rename")
    ("u" dired-upcase "upcase"))

  (bind-keys :map dired-mode-map
             ("[" . backward-page)
             ("]" . forward-page)
             ("z" . dired-do-flagged-delete)
             ("x" . god-mode-self-insert)
             ("e" . dired-toggle-read-only)
             ("SPC" . hydra-dired/body)))

(use-package package
  :config
  (defhydra hydra-package-menu (:color pink :hint nil)
    "
_k_↑ _p_rev    _U_pgrade      _d_elete   _/_ :filter  _H_ide       _r_evert
_j_↓ _n_ext    _~_: obsolete  _i_nstall  _S_ort       _(_: toggle
_<_  _>_       _z_: execute   _u_nmark   _?_: info    _q_uit
"
    ("SPC" nil)
    ("(" package-menu-toggle-hiding)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("?" package-menu-describe-package)
    ("H" package-menu-hide-package)
    ("S" tabulated-list-sort)
    ("U" package-menu-mark-upgrades)
    ("d" package-menu-mark-delete)
    ("/" hydra-package-menu-filter/body)
    ("g" revert-buffer)
    ("i" package-menu-mark-install)
    ("n" next-line)
    ("p" previous-line)
    ("q" quit-window :color blue)
    ("r" revert-buffer)
    ("u" package-menu-mark-unmark)
    ("z" package-menu-execute)
    ("~" package-menu-mark-obsolete-for-deletion)
    ("j" scroll-up-command)
    ("k" scroll-down-command))
  (defhydra hydra-package-menu-filter (:color pink :hint nil)
    "
Filter by: _k_eyword    _n_ame    _/_:clear
"
    ("SPC" nil)
    ("k" package-menu-filter-by-keyword)
    ("n" package-menu-filter-by-name)
    ("/" package-menu-clear-filter))

  (bind-keys :map package-menu-mode-map
             ("SPC" . hydra-package-menu/body)
             ("z" . package-menu-execute)
             ("x" . god-mode-self-insert)
             ("a" . god-mode-self-insert)
             ("e" . god-mode-self-insert)
             ("s" . consult-line)
             ("j" . scroll-up-command)
             ("k" . scroll-down-command)))

(use-package smerge-mode
  :bind ("C-x m" . hydra-smerge/body)
  :config
  (defhydra hydra-smerge
    (:color red :hint nil :pre (smerge-start-session))
    "
^Move^‗‗‗‗‗‗^Keep^‗‗‗‗‗‗‗‗‗^Diff^‗‗‗‗‗‗^Pair^‗‗‗‗‗‗‗‗‗‗
_n_ext      _b_ase         _R_efine    _<_: base-upper
_p_rev      _l_ower        _E_diff     _=_: upper-lower
^ ^         _u_pper        _C_ombine   _>_: base-lower
^ ^         _a_ll          _r_esolve
_q_uit      _RET_: current
"
    ("RET" smerge-keep-current)
    ("C"   smerge-combine-with-next)
    ("E"   smerge-ediff)
    ("R"   smerge-refine)
    ("a"   smerge-keep-all)
    ("b"   smerge-keep-base)
    ("l"   smerge-keep-lower)
    ("n"   smerge-next)
    ("p"   smerge-prev)
    ("r"   smerge-resolve)
    ("u"   smerge-keep-upper)
    ("<"   smerge-diff-base-upper)
    ("="   smerge-diff-upper-lower)
    (">"   smerge-diff-base-lower)
    ("q"   nil :color blue)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq with-editor-mode-lighter "")

  ;; missing variables from emacs 28
  (unless (boundp 'project-switch-commands)
    (setq project-switch-commands nil))

  (bind-keys :map magit-mode-map
             ("[" . magit-section-backward-sibling)
             ("]" . magit-section-forward-sibling)
             ("*" . god-mode-self-insert)
             ("x" . god-mode-self-insert)))

(use-package eldoc :diminish eldoc-mode
  :commands eldoc-mode)

(use-package bug-reference
  :commands bug-reference-mode
  :config
  (defun z-bug-to-link ()
    "Convert text captured from bug-reference-bug-regexp into links."
    (let ((m (match-string 2)))
      (if (s-ends-with? "@" m)
          (concat "teams/" (s-chop-suffix "@" m))
        (concat "http://" m))))
  (put 'z-bug-to-link 'bug-reference-url-format 't)
  (setq bug-reference-url-format 'z-bug-to-link
        bug-reference-bug-regexp
        "\\(\\b\\)\\(b/[0-9]+\\|c[rl]/[0-9]+\\|t/[0-9]+\\|\\(g\\|go\\|goto\\)/[-a-zA-z0-9_]+\\|[a-z]+@\\)"))

(use-package rainbow-identifiers
  :commands (rainbow-identifiers-mode)
  :config
  (setq rainbow-identifiers-choose-face-function
        'rainbow-identifiers-cie-l*a*b*-choose-face
        rainbow-identifiers-cie-l*a*b*-lightness 85
        rainbow-identifiers-cie-l*a*b*-saturation 8))

;; --------------------------------------------------
(use-package yasnippet :demand ;; :ensure
  :diminish yas-global-mode yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-prompt-functions
        '(yas-completing-prompt yas-no-prompt)
        yas-wrap-around-region t)

  (defun z-snippet-mode-hook ()
    ;; turn off minor modes derived from text-mode
    (auto-fill-mode -1)
    (abbrev-mode -1)
    (flyspell-mode -1))
  (add-hook 'snippet-mode-hook #'z-snippet-mode-hook)
  (yas-global-mode))

(defun z-re-backward (re count)
  "Search RE backward, return COUNT submatch.  Used in snippets."
  (save-excursion
    (save-match-data
      (when (re-search-backward re (point-min) t)
          (match-string count)))))


;; faces for mode-line status
(defface status-warning
  '((t (:foreground "#D8B080")))
  "Face for flycheck status: warnings" :group 'flycheck)
(defface status-ok
  '((t (:foreground "#98DC98")))
  "Face for flycheck status: OK" :group 'flycheck)
(defface status-error
  '((t (:foreground "#D88080")))
  "Face for flycheck status: error" :group 'flycheck)
(defface status-info
  '((t (:foreground "#80D8D8")))
  "Face for flycheck status: error" :group 'flycheck)

(defun z-status-count (s count)
  "Concat S and COUNT, except when COUNT is nil or 0, return empty string."
  (cond ((or (not count) (= 0 count)) "")
        ((> count 0) (concat s (number-to-string count)))
        (:else "")))

(defun z-status-str (nerror nwarning ninfo)
  "Return mode-line string for NERROR NWARNING NINFO counts."
  (or nerror (setq nerror 0))
  (or nwarning (setq nwarning 0))
  (or ninfo (setq ninfo 0))
  (if (and (= 0 nerror) (= 0 nwarning) (= 0 ninfo))
      (propertize "✔" 'face 'status-ok)
    (list ""
     (propertize (z-status-count "✖" nerror)
                 'face 'status-error)
     (if (and (< 0 nwarning) (< 0 nerror)) "/" "")
     (propertize (z-status-count "▴" nwarning)
                 'face 'status-warning)
     (if (and (or (< 0 nwarning) (< 0 nerror)) (< 0 ninfo)) "/" "")
     (propertize (z-status-count "•" ninfo)
                 'face 'status-info))))

(use-package flymake
  :commands (flymake-mode)
  :config

  (defun z-flymake-mode-line ()
    (let* ((running (flymake-running-backends))
           (disabled (flymake-disabled-backends))
           (reported (flymake-reporting-backends))
           (diags-by-type (make-hash-table))
           (all-disabled (and disabled (null running)))
           (some-waiting (cl-set-difference running reported)))
      (maphash (lambda (_b state)
                 (mapc (lambda (diag)
                         (push diag
                               (gethash (flymake--diag-type diag)
                                        diags-by-type)))
                       (flymake--backend-state-diags state)))
               flymake--backend-state)
      (cond (all-disabled "¿")
            (some-waiting (propertize "✔" 'face 'status-warning))
            (:else (z-status-str (length (gethash :error diags-by-type))
                             (length (gethash :warning diags-by-type))
                             (length (gethash :note diags-by-type)))))))

  (setq flymake--mode-line-format
        '(" " (:eval (z-flymake-mode-line))))

  (bind-keys :map flymake-mode-map
             ("M-g k"   . consult-flymake)
             ("M-g M-k" . flymake-show-diagnostics-buffer)
             ("M-g f"   . flymake-goto-next-error)
             ("M-g b"   . flymake-goto-prev-error)))

(use-package flycheck
  :commands (flycheck-mode)
  :config

  (defun z-flycheck-mode-line-text (&optional status)
    "Get a text using emoji to describe STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil.

This function is a drop-in replacement for the standard flycheck
function `flycheck-mode-line-status-text'.  If the selected emoji
cannot be displayed on the current frame,
`flycheck-mode-line-status-text' is automatically used as a
fallback."
    (let ((pick (pcase (or status flycheck-last-status-change)
                  (`finished
                   (if flycheck-current-errors
                       (let-alist (flycheck-count-errors flycheck-current-errors)
                         (z-status-str .error .warning .info))
                     (propertize "✔" 'face 'status-ok)))
                  (`running     (propertize "✔" 'face 'status-warning))
                  (`not-checked (propertize "✔" 'face 'status-error))
                  (`no-checker  "¿")
                  (`errored     "‼")
                  (`interrupted "⁉")
                  (`suspicious  "‽"))))
      (list " " pick)))
  (setq flycheck-mode-line '(:eval (z-flycheck-mode-line-text)))
  (bind-keys :map flycheck-mode-map
             ("M-g k"   . consult-flycheck)
             ("M-g M-k" . flycheck-list-errors)
             ("M-g f"   . flycheck-next-error)
             ("M-g b"   . flycheck-previous-error)))

;; --------------------------------------------------
;; modes

(use-package elisp-mode
  :config
  (defun z-setup-imenu-for-elisp ()
    "Recognize `use-package` and defhydra in imenu, for init files."
    (let ((emacsd (expand-file-name "~/.emacs.d/lisp/"))
          (initel (expand-file-name "init.el" "~/.emacs.d")))
      (when (and buffer-file-name
                 (or (string= buffer-file-name initel)
                     (string-match (rx-to-string `(: bos ,emacsd) t)
                                   buffer-file-name)))
        (add-to-list
         'imenu-generic-expression
         '("Packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2))
        (add-to-list
         'imenu-generic-expression
         '(nil "^\\s-*(\\(defhydra\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)))))

  (defun z-elisp-mode-hook ()
    (z-setup-imenu-for-elisp))
  (add-hook 'emacs-lisp-mode-hook #'z-elisp-mode-hook)
  (bind-keys :map emacs-lisp-mode-map
             ("M-L" . string-inflection-kebab-case))
  (bind-keys :map lisp-interaction-mode-map
             ("C-j")
             ("M-L" . string-inflection-kebab-case)))

(use-package cc-mode
  :config
  (setq c-electric-pound-behavior '(alignleft)) ;make a #define left-aligned
  (bind-key "RET" 'newline-and-indent c-mode-base-map)

  (defun z-c++-mode-hook ()
    (setq flycheck-clang-language-standard "c++14"
          flycheck-gcc-language-standard "c++14")
    (abbrev-mode -1)
    (require 'clang-format nil 't))
  (add-hook 'c++-mode-hook #'z-c++-mode-hook))

(use-package clang-format
  :commands z-maybe-clang-format
  :config
  (defun z-maybe-clang-format ()
    (when (eq major-mode 'c++-mode)
      (clang-format-buffer)))

  (add-hook 'before-save-hook #'z-maybe-clang-format))

(use-package go-mode
  :config
  ;; treat _ as symbol but not word, so that forward-word stops at it.
  (modify-syntax-entry ?_ "." go-mode-syntax-table)

  (defun z-go-mode-hook ()
    (setq tab-width 4)
    ;;(flycheck-mode)
    ;;(flycheck-select-checker 'go-gofmt)
    ;; (company-mode 1)
    ;;(go-eldoc-setup)
    )
  (add-hook 'go-mode-hook #'z-go-mode-hook))

(use-package js
  :config
  (bind-keys :map js-mode-map
             ("M-." . nil)
             ("C-x ." . js-find-symbol)))

(use-package haskell-mode
  :config
  (defun z-haskell-mode-hook ()
    (haskell-indentation-mode))
  ;;(setq haskell-font-lock-symbols 't)
  (add-hook 'haskell-mode-hook #'z-haskell-mode-hook))

(use-package julia-repl
  :hook (julia-mode . julia-repl-mode)
  :config
  (setq julia-repl-captures
        (list (kbd "M-x") (kbd "<home>"))))

;; --------------------------------------------------
;; ess

;; (use-package ess-julia
;;   :config
;;   (setq inferior-julia-program "~/bin/julia"))

(use-package ess-mode
  :bind ("<f5>" . z-switch-to-R)
  :defines ess-local-process-name
  :functions ess-debug-command-next
    ess-eval-line-and-step ess-eval-linewise
  :config
  (defun z-switch-to-R ()
    "Go to R session or create one if none exists"
    (interactive)
    (let ((b (or (get-buffer "*R*")
                (cl-find-if (lambda (b)
                               (eq (buffer-local-value 'major-mode b) 'inferior-ess-r-mode))
                            (buffer-list)))))
      (if b (switch-to-buffer b)
        (let ((ess-ask-for-ess-directory nil)
              (ess-directory "~/Projects"))
          (run-ess-r)))))

  (defun ess-smart-pipe (arg)
    "Similar to `ess-insert-assign', but insert %>% instead."
    (interactive "p")
    (let ((ess-assign-list '(" %>% ")))
      (ess-insert-assign arg)))

  (defun ess-debug-next-or-eval-line ()
    (interactive)
    (let ((proc (and ess-local-process-name
                     (get-process ess-local-process-name))))
      (if (and proc
               (or (process-get proc 'dbg-active)
                   (process-get proc 'is-recover)))
          (ess-debug-command-next)
        (ess-eval-line-and-step))))

  (defun ess-render-markdown ()
    (interactive)
    (ess-eval-linewise
     (concat "render(\"" buffer-file-name
             "\", output_dir = getwd())") nil 'eob))

  (defun z-ess-mode-hook ()
    (when (string-match "\\.Rmd\\'" buffer-file-name)
      (setq-local page-delimiter "^```\\({.*}\\)?$"))
    (setq-local tab-always-indent 'complete)
    (rainbow-delimiters-mode 1)
    (prettify-symbols-mode 1))

  (add-hook 'ess-mode-hook #'z-ess-mode-hook)

  (setq ess-r-prettify-symbols
        '(("%>%" . ?↦)
          ("%T>%" . ?↧) ;↴
          ("%<>%" . ?⇄) ;⇋⇌⇆
          ("%<-%" ?↢)   ;destructuring assignment in zeallot
          ("<-" . ?←)
          ("->" . ?→)
          ("<=" . ?≤)
          (">=" . ?≥)
          ("!=" . ?≠)
          ("%in%" . ?∈)
          ("%*%" . ?×)
          ("function" . ?ƒ)))
  (setq ess-use-flymake nil
        ess-use-ido nil
        ess-busy-strings '("  " " ◐" " ◓" " ◑" " ◒")
        ess-assign-list '(" <- "))

  ;; Make imenu recognize Rmd sections and functions. The default did not
  ;; make much sense.
  (setq ess-imenu-S-generic-expression
        '(("Sections" "^\\s-*```{r \\(\\sw[a-zA-Z0-9_.]+\\)" 1)
          ("Functions" "^\\(.+\\)[      \n]*<-[         \n]*function[ ]*(" 1)))

  (bind-keys :map ess-mode-map
             ("<f7>" . ess-show-traceback)
             ("<f8>" . ess-debug-next-or-eval-line)
             ("<f9>" . ess-eval-function-or-paragraph-and-step)
             ("C-x <f8>" . ess-tracebug)
             ("C-c SPC" . ess-render-markdown)
             ("C-c C-m" . markdown-mode)
             ("_")                 ; unbind ess-smart-S-assign
             ("{") ("}")           ; unbind skeleton-pair-insert-maybe
             ("C-M-j")
             ("M-j")
             ("\\" . ess-smart-pipe)
             (";" . ess-cycle-assign)))

(use-package ess-help
  :config
  (defhydra hydra-ess-help (:color pink :hint nil)
    "
_k_↑  _p_rev   _[_ _]_: _s_ection _h_elp-on-obj  _v_ignettes _/_isearch
_j_↓  _n_ext   _<_ _>_: buf^^     _g_:revert^^     _a_propos      _i_ndex     _q_uit
eval: _f_unction  _l_ine  _r_egion
"
    ("/" isearch-forward)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("a" ess-display-help-apropos)
    ("f" ess-eval-function-or-paragraph-and-step)
    ("g" revert-buffer)
    ("h" ess-display-help-on-object)
    ("i" ess-display-package-index)
    ("j" scroll-up-command)
    ("k" scroll-down-command)
    ("l" ess-eval-line-and-step)
    ("[" ess-skip-to-previous-section)
    ("]" ess-skip-to-next-section)
    ("q" quit-window :color blue)
    ("r" ess-eval-region-and-go)
    ("s" hydra-ess-help-s/body :color blue)
    ("v" ess-display-vignettes)
    ("n" next-line)
    ("p" previous-line)
    ("SPC" nil))

  (defhydra hydra-ess-help-s (:color pink :hint nil)
    "
section: _a_rguments  _d_escription  _D_e_t_ails  _e_xamples  _n_ote  _r_eferences  _s_ee-also  _u_sage  _v_alue[s]
"
    ("a" ess-skip-to-help-section)
    ("d" ess-skip-to-help-section)
    ("D" ess-skip-to-help-section)
    ("t" ess-skip-to-help-section)
    ("e" ess-skip-to-help-section)
    ("n" ess-skip-to-help-section)
    ("r" ess-skip-to-help-section)
    ("s" ess-skip-to-help-section)
    ("u" ess-skip-to-help-section)
    ("v" ess-skip-to-help-section)
    ("SPC" nil))
  (bind-keys :map ess-help-mode-map
             ("<f8>" . ess-eval-line-and-step)
             ("<f9>" . ess-eval-function-or-paragraph-and-step)
             ("SPC" . hydra-ess-help/body)
             ("j" . scroll-up-command)
             ("k" . scroll-down-command)
             ("[" . ess-skip-to-previous-section)
             ("]" . ess-skip-to-next-section)
             ("n" . next-line)
             ("p" . previous-line)
             ("x" . god-mode-self-insert)
             ("c" . god-mode-self-insert)))


(use-package ess-r-mode
  :mode (("\\.Rmd\\'" . ess-r-mode))
  :config
  ;; For Rmd editing, do not treat ` as quote.
  (modify-syntax-entry ?` "." ess-r-mode-syntax-table)
  (modify-syntax-entry ?% "." ess-r-mode-syntax-table)

  (defun z-inferior-ess-mode-hook ()
    (setq prettify-symbols-alist ess-r-prettify-symbols)
    (prettify-symbols-mode 1)
    (setq-local scroll-margin 0)
    (setq-local comint-move-point-for-output t))
  (add-hook 'inferior-ess-r-mode-hook #'z-inferior-ess-mode-hook)

  (bind-keys :map inferior-ess-r-mode-map
             ("\C-cw" . ess-execute-screen-options)
             ("<f7>" . ess-show-R-traceback)
             ("C-x <f8>" . ess-tracebug)
             ("_")
             ("\\" . ess-smart-pipe)
             (";" .  ess-cycle-assign)))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-header-scaling 't
        markdown-header-scaling-values
        '(1.86 1.39 1.24 1 1 0.93))
  :config
  (bind-keys :map markdown-mode-map
             ("C-M-a" . markdown-previous-visible-heading)
             ("C-M-e" . markdown-next-visible-heading)
             ("C-c C-m" . ess-r-mode)))

;; ----------------------------------------------------------
(use-package gdb-mi
  :config
  (defun z-gdb-mode-hook () (setq gdb-many-windows t))
  (add-hook 'gdb-mode-hook #'z-gdb-mode-hook))

(use-package man
  :config
  (defhydra hydra-man (:color pink :hint nil)
    "
_k_↑ _<__>_ top/bottom  S/tab:button   _g_oto sec  _m_an        _K_ill
_j_↓ _[__]_ section                    _s_ee also  _r_eference  _q_uit
"
    ("SPC" nil :exit t)
    ("j" scroll-up-command)
    ("k" scroll-down-command)
    ("K" Man-kill :exit t)
    ("q" quit-window :exit t)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("[" Man-previous-section)
    ("]" Man-next-section)
    ("g" Man-goto-section)
    ("s" Man-goto-see-also-section)
    ("m" man)
    ("r" Man-follow-manual-reference))
  (bind-keys :map Man-mode-map
             ("SPC" . hydra-man/body)
             ("j" . scroll-up-command)
             ("k" . scroll-down-command)
             ("K" . Man-kill)
             ("[" . Man-previous-section)
             ("]" . Man-next-section)
             ("{" . backward-paragraph)
             ("}" . forward-paragraph)
             ("x" . god-mode-self-insert)))

(use-package info
  :config
  (defhydra hydra-info (:color pink :hint nil)
    "
^ ^      ^^^^Reference   ^^History       ^^Tree
_k_↑     ^^^^S/TAB:↔     _l_: back       _n_ext   _d_irectory _T_OC
_j_↓     ^^_f_ollow      _r_: forward    _p_rev   _<__>_ first/last
^ ^      ^^_m_enu        _L_: history    _u_p     _[__]_ back/forward
"
    ("q" quit-window :exit t)
    ("SPC" nil :exit t)
    ("n" Info-next)
    ("p" Info-prev)
    ("u" Info-up)
    ("m" Info-menu)
    ("d" Info-directory)
    ("<" Info-top-node)
    (">" Info-final-node)
    ("[" Info-backward-node)
    ("]" Info-forward-node)
    ("f" Info-follow-reference)
    ("l" Info-history-back)
    ("r" Info-history-forward)
    ("L" Info-history)
    ("T" Info-toc)
    ("j" Info-scroll-up)
    ("k" Info-scroll-down))
  (bind-keys :map Info-mode-map
             ("SPC" . hydra-info/body)
             ("j" . Info-scroll-up)
             ("k" . Info-scroll-down)
             ("M-n")                    ; this was clone-buffer
             ("{" . backward-paragraph)
             ("}" . forward-paragraph)
             ("x" . god-mode-self-insert)))

(use-package help-mode
  :config
  (defhydra hydra-help (:color pink :hint nil)
    "
_k_↑    _<_ _>_ top/bottom   _l_: back
_j_↓    ^^^^S/tab: buttons   _r_: forward
"
    ("SPC" nil :exit t)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("g" revert-buffer)
    ("h" describe-mode)
    ("l" help-go-back)
    ("q" quit-window :exit t)
    ("r" help-go-forward)
    ("k" scroll-down-command)
    ("j" scroll-up-command))

  (bind-keys :map help-mode-map
             ("SPC" . hydra-help/body)
             ("k" . scroll-down-command)
             ("j" . scroll-up-command)
             ("n" . next-line)
             ("p" . previous-line)
             ("{" . backward-paragraph)
             ("}" . forward-paragraph)
             ("x" . god-mode-self-insert)))

(use-package server :diminish (server-buffer-clients . " #"))
(add-hook 'after-init-hook 'server-start)

(use-package edit-server :ensure
  :diminish (edit-server-edit-mode)
  :init
  (add-hook 'after-init-hook 'edit-server-start)
  :config
  (setq edit-server-new-frame nil
        edit-server-url-major-mode-alist
        '(("mail\\.google\\.com" . html-mode)
          ("snippets\\.googleplex\\.com" . markdown-mode)
          ("b\\.corp\\.google\\.com" . gfm-mode))))

(use-package calc
  :bind (("M-*" . calc-dispatch)))

(use-package calc-ext
  :config

  (defvar hydra-calc-paused nil
    "Record if hydra-calc has been paused")

  (defhydra hydra-calc (:color pink :hint nil)
    "
Notations: 3.14e6  _23=-23  3:4=¾  5:2:3=5⅔  16#12C=0x12C  [1..4)=interval
----------------  scr_o_ll: _{_↑  _<_ _>_  ↓_}_ ---------------
_U_n/_D_o        x_!_    _Q_:√    _H_yper    _S_in   ^^(2,4)=2+4i  ^^Vector    _w_hy      _y_ank-to-buf
_`_edit^^        _&_:x⁻¹ _B_:log  _I_nv      _C_os   ^^(2;4)=2e⁴ⁱ  ^^[1,2,3]   _M_:+recur
_K_eep arg^^     _%_mod  _L_n     _F_loor    _T_an   con_J_ z̄      _|_concat   _O_ption
_~_num-prefix^^  _A_bs   _E_xp    _R_ound    _f_unc  ar_G_:∠z      ^^          _p_recision  _h_elp
_=_eval-_N_um    _n_±    _P_i:π   _a_lgebra  ^^      _t_rail/time  _c_onvert   _m_ode       _i_nfo
"
    ("SPC" nil)
    ("!"  calc-factorial)
    ("%"  calc-mod)
    ("&"  calc-inv)
    ("<"  calc-scroll-left)
    ("="  calc-evaluate)
    (">"  calc-scroll-right)
    ("A"  calc-abs)
    ("B"  calc-log)
    ("C"  calc-cos)
    ("D"  calc-redo)
    ("E"  calc-exp)
    ("F"  calc-floor)
    ("G"  calc-argument)
    ("H"  calc-hyperbolic)
    ("I"  calc-inverse)
    ("J"  calc-conj)
    ("K"  calc-keep-args)
    ("L"  calc-ln)
    ("M"  calc-more-recursion-depth)
    ("N"  calc-eval-num)
    ("O"  calc-option)
    ("P"  calc-pi)
    ("Q"  calc-sqrt)
    ("R"  calc-round)
    ("S"  calc-sin)
    ("T"  calc-tan)
    ("U"  calc-undo)
    ;; ("V"               Prefix Command)
    ("X"               calc-call-last-kbd-macro)
    ;; ("Y"               Prefix Command)
    ;; ("Z"               Prefix Command)
    ("`"               calc-edit)
    ("a"               (progn (setq hydra-calc-paused t)
                              (hydra-calc-a/body)) :exit t)
    ;; ("b"               Prefix Command)
    ("c"               (progn (setq hydra-calc-paused t)
                              (hydra-calc-c/body)))
    ;; ("d"               Prefix Command)
    ("f"               (progn (setq hydra-calc-paused t)
                              (hydra-calc-f/body)) :exit t)
    ;; ("g"               Prefix Command)
    ("h"               calc-help-prefix)
    ("i"               calc-info)
    ;; ("j"               Prefix Command)
    ;; ("k"               Prefix Command)
    ;; ("l"               Prefix Command)
    ("m"               (progn (setq hydra-calc-paused t)
                              (hydra-calc-m/body)))
    ("n"               calc-change-sign)
    ("o"               calc-realign)
    ("p"               calc-precision)
    ("q"               calc-quit :exit t)
    ;; ("r"               Prefix Command)
    ("s"               (progn (setq hydra-calc-paused t)
                              (hydra-calc-s/body)) :exit t)
    ("t"               (progn (setq hydra-calc-paused t)
                              (hydra-calc-t/body)) :exit t)
    ;; ("u"               Prefix Command)
    ;; ("v"               Prefix Command)
    ("w"               calc-why)
    ("x"               calc-execute-extended-command)
    ("y"               calc-copy-to-buffer)
    ;; ("z"               Prefix Command)
    ("{"               calc-scroll-down)
    ("|"               calc-concat)
    ("}"               calc-scroll-up)
    ("~"               calc-num-prefix))

  (defhydra hydra-calc-a (:color blue :hint nil
                                 :after-exit (when hydra-calc-paused
                                               (setq hydra-calc-paused nil)
                                               (hydra-calc/body)))
    "
^^^‗‗‗‗Logical‗‗‗‗^^^   ‗‗^^Poly‗‗     ^^            ^^          ^^     ‗‗‗‗^^Numerical‗‗‗‗
_!_:¬   _._:remove=^^   ^_%_:rem       _f_actor     _s_implify/_e_xtd   _*_:∏      ^_R_oot              _M_ap eqn^^   _a_part
_&_:∧   _:_if _#_:≠     _\\_:div       _i_:∫        _c_ollect^^         _+_:∑      ^_F_it curve         mi_N_/ma_X_   su_b_stitute
_|_:∨   _{_:∈      ^^   ^_P_oly-roots  _d_/dx       _n_ormalize^^       _-_:alt∑   ^_S_olve             _m_atch^^   _A_bs
_[_:≤   _]_:≥      ^^   ^_g_cd         ̲:subᵢ^^     _r_ewrite   ^^      _I_:∫dx    _\"_:expand formula  _T_abulate^^
_<_ _=_ _>_             ^^^            _t_aylor     e_x_pand ^^         alg-e_v_aluate  _p_oly-interp
"
    ("SPC" nil)
    ("!"  calc-logical-not) ("\"" calc-expand-formula) ("#"  calc-not-equal-to)
    ("%"  calc-poly-rem) ("&"  calc-logical-and) ("*"  calc-product) ("+"  calc-summation)
    ("-"  calc-alt-summation) ("."  calc-remove-equal) ("/"  calc-poly-div-rem) (":"  calc-logical-if)
    ("<"  calc-less-than) ("="  calc-equal-to) (">"  calc-greater-than) ("?"  calc-a-prefix-help)
    ("A"  calc-abs) ("F"  calc-curve-fit) ("I"  calc-num-integral) ("M"  calc-map-equation)
    ("N"  calc-find-minimum) ("P"  calc-poly-roots) ("R"  calc-find-root) ("S"  calc-solve-for)
    ("T"  calc-tabulate) ("X"  calc-find-maximum) ("["  calc-less-equal) ("\\" calc-poly-div)
    ("]"  calc-greater-equal) ("_"  calc-subscript)
    ("a"  calc-apart)
    ("b"  calc-substitute)
    ("c"  calc-collect)
    ("d"  calc-derivative)
    ("e"  calc-simplify-extended)
    ("f"  calc-factor)
    ("g"  calc-poly-gcd)
    ("i"  calc-integral)
    ("m"  calc-match)
    ("n"  calc-normalize-rat)
    ("p"  calc-poly-interp)
    ("r"  calc-rewrite)
    ("s"  calc-simplify)
    ("t"  calc-taylor)
    ("v"  calc-alg-evaluate)
    ("x"  calc-expand)
    ("{"  calc-in-set)
    ("|"  calc-logical-or))

  (defhydra hydra-calc-t (:color blue :hint nil
                                 :after-exit (when hydra-calc-paused
                                               (setq hydra-calc-paused nil)
                                               (hydra-calc/body)))
    "
‗‗‗‗^^^^Trail^^^^‗‗‗‗‗‗‗  ^^         ^^^^         ^^            ‗‗‗‗^^Time^^‗‗‗‗
_[_ _]_ first/last^^^^    _d_isplay  _m_arker^^   0-9:store^^   _-_/_+_ buzn day  _Y_ear    _T_an
_p_rev/_n_ext ^^^^        _h_ere     _k_ill^^     _._:vector    _C_onvert T_Z_    _M_onth   _U_nix
_<_ _>_ scroll ^^^^       _i_n       _y_ank^^     ^^            _N_ow^^           _W_eek    _P_art
_{_ _}_ _b_ack/_f_orward  _o_ut      _r_/_s_earch ^^            _I_nc month^^     _D_ate    _J_ulian
"
    ("SPC" nil)
    ("+"             calc-business-days-plus)
    ("-"             calc-business-days-minus)
    ("."             calc-full-trail-vectors)
    ("<"             calc-trail-scroll-left)
    (">"             calc-trail-scroll-right)
    ("?"             calc-t-prefix-help)
    ("C"             calc-convert-time-zones)
    ("D"             calc-date)
    ("I"             calc-inc-month)
    ("J"             calc-julian)
    ("M"             calc-new-month)
    ("N"             calc-now)
    ("P"             calc-date-part)
    ("T"             calc-tan)
    ("U"             calc-unix-time)
    ("W"             calc-new-week)
    ("Y"             calc-new-year)
    ("Z"             calc-time-zone)
    ("["             calc-trail-first)
    ("]"             calc-trail-last)
    ("b"             calc-trail-backward)
    ("d"             calc-trail-display)
    ("f"             calc-trail-forward)
    ("h"             calc-trail-here)
    ("i"             calc-trail-in)
    ("k"             calc-trail-kill)
    ("m"             calc-trail-marker)
    ("n"             calc-trail-next)
    ("o"             calc-trail-out)
    ("p"             calc-trail-previous)
    ("r"             calc-trail-isearch-backward)
    ("s"             calc-trail-isearch-forward)
    ("y"             calc-trail-yank)
    ("{"             calc-trail-backward)
    ("}"             calc-trail-forward))

  (defhydra hydra-calc-f (:color blue :hint nil
                                 :after-exit (when hydra-calc-paused
                                               (setq hydra-calc-paused nil)
                                               (hydra-calc/body)))
    "
_A_bs²      _F_loor^^     _h_=√x²+y²   _b_eta       _s_ign^^         fp=m*10ᵉ
_e_rf       _I_nt-log^^   arc_T_an2    _g_amma      bessel-_j_/_y_   _M_antissa
_E_xp-1     int-s_Q_rt^^  _i_m         inc-_B_eta   _[_-1 _]_+1      e_X_ponent
_L_n+1      mi_n_/ma_x_   _r_e         inc-_G_amma  ^^^^             _S_cale
"
    ("SPC" nil :exit t)
    ("A"             calc-abssqr)
    ("B"             calc-inc-beta)
    ("E"             calc-expm1)
    ("F"             calc-floor)
    ("G"             calc-inc-gamma)
    ("I"             calc-ilog)
    ("L"             calc-lnp1)
    ("M"             calc-mant-part)
    ("Q"             calc-isqrt)
    ("S"             calc-scale-float)
    ("T"             calc-arctan2)
    ("X"             calc-xpon-part)
    ("["             calc-decrement)
    ("]"             calc-increment)
    ("b"             calc-beta)
    ("e"             calc-erf)
    ("g"             calc-gamma)
    ("h"             calc-hypot)
    ("i"             calc-im)
    ("j"             calc-bessel-J)
    ("n"             calc-min)
    ("r"             calc-re)
    ("s"             calc-sign)
    ("x"             calc-max)
    ("y"             calc-bessel-Y))

  (defhydra hydra-calc-m (:color blue :hint nil
                                 :after-exit (when hydra-calc-paused
                                               (setq hydra-calc-paused nil)
                                               (hydra-calc/body)))
    "
‗‗Simplify^^^^‗‗    _a_lgebra    _d_eg°   auto-re_C_ompute    _g_et modes
N_O_      _A_lg     _t_otal↵     _r_adπ   settings _F_ile     save _m_odes
_N_um     _E_xt     _s_mbolic    _h_ms    _M_ore recur-dep    _X_:load all
bas_I_c   _U_nits   _p_olar      _i_nf∞   _S_hift prefix      always e_x_tensions
_B_in     _D_efault _v_:matrix   _f_rac   _w_orking           mode _R_ecord
                                                             _e_mbedded-preserve modes
"
    ("A"             calc-alg-simplify-mode)
    ("B"             calc-bin-simplify-mode)
    ("C"             calc-auto-recompute)
    ("D"             calc-default-simplify-mode)
    ("E"             calc-ext-simplify-mode)
    ("F"             calc-settings-file-name)
    ("I"             calc-basic-simplify-mode)
    ("M"             calc-more-recursion-depth)
    ("N"             calc-num-simplify-mode)
    ("O"             calc-no-simplify-mode)
    ("R"             calc-mode-record-mode)
    ("S"             calc-shift-prefix)
    ("U"             calc-units-simplify-mode)
    ("X"             calc-load-everything)
    ("a"             calc-algebraic-mode)
    ("d"             calc-degrees-mode)
    ("e"             calc-embedded-preserve-modes)
    ("f"             calc-frac-mode)
    ("g"             calc-get-modes)
    ("h"             calc-hms-mode)
    ("i"             calc-infinite-mode)
    ("m"             calc-save-modes)
    ("p"             calc-polar-mode)
    ("r"             calc-radians-mode)
    ("s"             calc-symbolic-mode)
    ("t"             calc-total-algebraic-mode)
    ("v"             calc-matrix-mode)
    ("w"             calc-working)
    ("x"             calc-always-load-extensions))


  (defhydra hydra-calc-c (:color blue :hint nil
                                 :after-exit (when hydra-calc-paused
                                               (setq hydra-calc-paused nil)
                                               (hydra-calc/body)))
    "
_d_egree  _%_:percent  _p_oloar
_r_adian  _F_raction   _c_lean
_h_ms     _f_loat      _C_os
"
    ("%" calc-convert-percent)
    ("C" calc-cos)
    ("F" calc-fraction)
    ("c" calc-clean)
    ("d" calc-to-degrees)
    ("f" calc-float)
    ("h" calc-to-hms)
    ("p" calc-polar)
    ("r" calc-to-radians))

  (defhydra hydra-calc-s (:color blue :hint nil
                                 :after-exit (when hydra-calc-paused
                                               (setq hydra-calc-paused nil)
                                               (hydra-calc/body)))
    "
‗‗_s_tore‗‗   in_t_o        ‗v←_m_ap(v)‗^^^^^^  ‗‗Special Variables‗‗
0..9 quick^^  _i_nsert      _&_:1/v ^^ ^^ ^^    _A_lgSimpRules  _H_olidays    _L_ineStyles
_:_assign     _k_onstant    _+__-__*__/_        e_X_tSimpRules  _I_ntegLimit  _P_ointStyles
_=_evalto     _l_et         _[_+1 _]_-1 ^^ ^^   _E_valRules     _U_nits       Plot_R_ejects
_r_ecall      _p_ermanent   _|_concat ^^ ^^ ^^  _F_itRules      _D_ecls
_c_opy        e_x_change    _n_eg^^ ^^ ^^       _G_enCount      _T_imeZone
_d_eclare     _u_nstore     _e_dit^^ ^^ ^^
"
    ("&"             calc-store-inv)
    ("*"             calc-store-times)
    ("+"             calc-store-plus)
    ("-"             calc-store-minus)
    ("/"             calc-store-div)
    (":"             calc-assign)
    ("="             calc-evalto)
    ("A"             calc-edit-AlgSimpRules)
    ("D"             calc-edit-Decls)
    ("E"             calc-edit-EvalRules)
    ("F"             calc-edit-FitRules)
    ("G"             calc-edit-GenCount)
    ("H"             calc-edit-Holidays)
    ("I"             calc-edit-IntegLimit)
    ("L"             calc-edit-LineStyles)
    ("P"             calc-edit-PointStyles)
    ("R"             calc-edit-PlotRejects)
    ("S"             calc-sin)
    ("T"             calc-edit-TimeZone)
    ("U"             calc-edit-Units)
    ("X"             calc-edit-ExtSimpRules)
    ("["             calc-store-decr)
    ("]"             calc-store-incr)
    ("^"             calc-store-power)
    ("c"             calc-copy-variable)
    ("d"             calc-declare-variable)
    ("e"             calc-edit-variable)
    ("i"             calc-insert-variables)
    ("k"             calc-copy-special-constant)
    ("l"             calc-let)
    ("m"             calc-store-map)
    ("n"             calc-store-neg)
    ("p"             calc-permanent-variable)
    ("r"             calc-recall)
    ("s"             calc-store)
    ("t"             calc-store-into)
    ("u"             calc-unstore)
    ("x"             calc-store-exchange)
    ("|"             calc-store-concat))


  (bind-keys :map calc-mode-map
             ("a SPC" . hydra-calc-a/body)
             ("t SPC" . hydra-calc-t/body)
             ("f SPC" . hydra-calc-f/body)
             ("m SPC" . hydra-calc-m/body)
             ("c SPC" . hydra-calc-c/body)
             ("s SPC" . hydra-calc-s/body)
             ("SPC" . hydra-calc/body)
             ("M-n" . calc-trail-next)
             ("M-p" . calc-trail-previous)))

(use-package shell
  :config
  (defun z-shell-mode-hook ()
    (setq dirtrack-list
          '(":\\[[0-9;]*m\\([^]*\\)" 1))
    (shell-dirtrack-mode -1)
    (dirtrack-mode 1))
  (add-hook 'shell-mode-hook #'z-shell-mode-hook)

  (bind-keys :map shell-mode-map
             ("C-M-a" . comint-previous-prompt)
             ("C-M-e" . comint-next-prompt)))

(use-package comint
  :config
  (ansi-color-for-comint-mode-on)
  (setq comint-scroll-to-bottom-on-output 't
        comint-scroll-show-maximum-output nil))

(provide 'init-mode)
;;; init-mode.el ends here
