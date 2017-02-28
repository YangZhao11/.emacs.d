; -*- coding: utf-8; lexical-binding: t -*-

(eval-when-compile
  (require 'use-package)
  (require 'hydra))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook #'abbrev-mode)
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook #'turn-on-flyspell)

(use-package bookmark
  :config
  (defhydra hydra-bookmark-bmenu (:color pink :hint nil)
    "
_m_ark     _u_nmark    _1_ window  _v_isit    _s_ave/_l_oad    _a_nnotation
_d_elete   _DEL_: back _2_ window  _r_ename   _t_oggle fname^^ _A_ll
e_x_ecute  _RET_: go   _o_ther win _R_elocate _w_here^^        _e_dit
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
    ("x" bookmark-bmenu-execute-deletions)
    ("s" bookmark-bmenu-save)
    ("l" bookmark-bmenu-load)
    ("u" bookmark-bmenu-unmark)
    ("DEL" bookmark-bmenu-backup-unmark)
    ("a" bookmark-bmenu-show-annotation)
    ("A" bookmark-bmenu-show-all-annotations)
    ("e" bookmark-bmenu-edit-annotation))
  (bind-keys :map bookmark-bmenu-mode-map
             ("SPC" . hydra-bookmark-bmenu/body))
)

(use-package view
  :config
  (defhydra hydra-view (:color pink :hint nil)
    "
^^pg/set ^^line ^^half _g_o/_%_ ^^register  ^^mark^^       _s_earch/_r_    _q_uit/_Q_
_k_↑ _w_   _K_    _u_p   _<_^^    _m_ark      _._ set/p_@_p  regex: _/_ ^\\  _c_ancel/_C_
_j_↓ _z_   _J_    _d_own _>_^^    _'_: goto   e_x_chg^^      again: _n_ _p_
"
    ("SPC" nil)
    ("j" View-scroll-page-forward)
    ("k" View-scroll-page-backward)
    ("z" View-scroll-page-forward-set-page-size)
    ("w" View-scroll-page-backward-set-page-size)
    ("J" View-scroll-line-forward)
    ("K" View-scroll-line-backward)
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
    ("n" View-search-last-regexp-forward)
    ("p" View-search-last-regexp-backward)
    ("m" point-to-register)
    ("'" register-to-point)
    ("." set-mark-command)
    ("@" View-back-to-mark)
    ("x" exchange-point-and-mark)
    ("q" View-quit :color blue)
    ("Q" View-quit-all :color blue)
    ("c" View-leave :color blue)
    ("C" View-kill-and-leave :color blue))
  (bind-keys :map view-mode-map
             ("SPC" . hydra-view/body)
             ("C-j" . nil)
             ("j" . View-scroll-page-forward)
             ("k" . View-scroll-page-backward)
             ("J" . View-scroll-line-forward)
             ("K" . View-scroll-line-backward)))

;; replace.el is not a real package
(defhydra hydra-occur (:color pink :hint nil)
  "
_p_rev^^   _RET_: goto      _e_dit
_n_ext^^   _o_ther window   %s(if next-error-follow-minor-mode \"⇅\" \"☐\") _f_ollow
_<_ _>_    _d_isplay
"
  ("SPC" nil)
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
           ("SPC" . hydra-occur/body))

(use-package grep
  :config
  (require 'wgrep)
  (defhydra hydra-grep (:color pink :hint nil)
  "
_k_ ↑^^  _p_rev^^  _<__>_  beg/end of buffer _RET_: goto  _e_dit
_j_ ↓^^  _n_ext^^  _{__}_: prev/next file    _d_isplay
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
             ("SPC" . hydra-grep/body)))

(use-package dired
  :bind (("C-x C-d" . dired)
         ("C-x C-j" . dired-jump))
  :config
  (require 'dired-x)
  (defhydra hydra-dired (:color pink :columns 3 :hint nil)
    "
^Mark^       ^Flag^        ^Emacs Op^      ^ ^              ^^File Op^^ (_e_dit)
^----^-------^----^--------^--------^------^-^--------------^^-------^^--^-^------
_*_: marks   _#_: temp     _Q_uery replace _F_ind marked    _!_shell_&_ _S_ymlink
_%_: regexp  _~_: backup   _A_: grep       _L_oad           ^^_C_opy    _H_ardlink
_u_n/_m_ark    _d_: this     _B_yte compile  _k_ill line      ^^_D_elete  ch_M_od
_t_oggle     _x_: delete   _v_iew          _w_: file name   ^^_R_ename  ch_O_wn
_U_nmark all _<_ _>_:dirline _o_ther window  redisp_l_ay      ^^_T_ouch   ch_G_rp
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
    ("Z" dired-do-compress "compress")
    ("^" dired-up-directory "up-directory")
    ("a" dired-find-alternate-file "find-alternate-file")
    ("c" dired-do-compress-to "compress-to")
    ("d" dired-flag-file-deletion)
    ("i" dired-maybe-insert-subdir "maybe-insert-subdir")
    ("j" dired-goto-file "goto-file")
    ("k" dired-do-kill-lines)
    ("l" dired-do-redisplay)
    ("o" dired-find-file-other-window)
    ("s" dired-sort-toggle-or-edit "sort-toggle-or-edit")
    ("t" dired-toggle-marks)
    ("v" dired-view-file)
    ("w" dired-copy-filename-as-kill)
    ("x" dired-do-flagged-delete)
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
             ("SPC" . hydra-dired/body)))

(defhydra hydra-package-menu (:color pink :hint nil)
  "
_k_ ↑ _p_rev    _U_pgrade      _d_elete   _f_ilter _H_ide       _r_efresh
_j_ ↓ _n_ext    _~_: obsolete  _i_nstall  _S_ort   _(_: toggle  _g_: revert
_<_   _>_       e_x_ecute      _?_: info  _u_nmark _q_uit
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
  ("f" package-menu-filter)
  ("g" revert-buffer)
  ("i" package-menu-mark-install)
  ("n" next-line)
  ("p" previous-line)
  ("q" quit-window :color blue)
  ("r" package-menu-refresh)
  ("u" package-menu-mark-unmark)
  ("x" package-menu-execute)
  ("~" package-menu-mark-obsolete-for-deletion)
  ("j" scroll-up-command)
  ("k" scroll-down-command))
(bind-keys :map package-menu-mode-map
           ("SPC" . hydra-package-menu/body)
           ("j" . scroll-up-command)
           ("k" . scroll-down-command))

(use-package smerge-mode
  :bind ("C-x m" . hydra-smerge/body)
  :config
  (defhydra hydra-smerge
    (:color red :hint nil
            :pre (smerge-mode 1))
    "
^Move^      ^Keep^         ^Diff^      ^Pair^
------------------------------------------------------
_n_ext      _b_ase         _R_efine    _<_: base-mine
_p_rev      _m_ine         _E_diff     _=_: mine-other
^ ^         _o_ther        _C_ombine   _>_: base-other
^ ^         _a_ll          _r_esolve
_q_uit      _RET_: current
"
    ("RET" smerge-keep-current)
    ("C"   smerge-combine-with-next)
    ("E"   smerge-ediff)
    ("R"   smerge-refine)
    ("a"   smerge-keep-all)
    ("b"   smerge-keep-base)
    ("m"   smerge-keep-mine)
    ("n"   smerge-next)
    ("o"   smerge-keep-other)
    ("p"   smerge-prev)
    ("r"   smerge-resolve)
    ("<"   smerge-diff-base-mine)
    ("="   smerge-diff-mine-other)
    (">"   smerge-diff-base-other)
    ("q"   nil :color blue)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq with-editor-mode-lighter "")
  (setq magit-completing-read-function 'ivy-completing-read)
  (defhydra hydra-magit-j (:color blue :hint nil)
"
_u_n/_s_taged   u_n_/_t_racked   un_p_ushed   un_f_etched   _z_: stashes"
    ("SPC" nil)
    ("n" magit-jump-to-untracked)
    ("t" magit-jump-to-tracked)
    ("s" magit-jump-to-staged)
    ("u" magit-jump-to-unstaged)
    ("p" hydra-magit-j-p/body)
    ("f" hydra-magit-j-f/body)
    ("z" magit-jump-to-stashes))
  (defhydra hydra-magit-j-p (:color blue :hint nil)
    "
jump to unpushed to: _p_ushremote  _u_pstream"
    ("SPC" nil)
    ("p" magit-jump-to-unpushed-to-pushremote)
    ("u" magit-jump-to-unpushed-to-upstream))
  (defhydra hydra-magit-j-f (:color blue :hint nil)
    "
jump to unfetched from: _p_ushremote  _u_pstream"
    ("SPC" nil nil)
    ("p" magit-jump-to-unpulled-from-pushremote)
    ("u" magit-jump-to-unpulled-from-upstream))
  (bind-keys :map magit-mode-map ("j SPC" . hydra-magit-j/body)))

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

(use-package color-identifiers-mode
  :diminish 'color-identifiers-mode
  :commands (color-identifiers-mode)
  :config
  (setq color-identifiers:min-color-saturation 0.1
        color-identifiers:max-color-saturation 0.3))

;; --------------------------------------------------
(use-package yasnippet :demand ;; :ensure
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (setq yas-prompt-functions
        '(yas-completing-prompt yas-no-prompt)
        yas-wrap-around-region t)
  (yas-global-mode)
  (diminish 'yas-minor-mode))

(defun z-re-backward (re count)
  "Search RE backward, return COUNT submatch.  Used in snippets."
  (save-excursion
    (save-match-data
      (when (re-search-backward re (point-min) t)
          (match-string count)))))

(use-package company :defer 't
  :diminish " ©"
  :commands (company-mode)
  :bind (("M-m" . company-complete))
  :config
  (setq company-idle-delay nil))

(use-package flycheck
  :commands (flycheck-mode)
  :config

  (defface flycheck-status-warning
    '((t (:foreground "#D8B080" :inherit mode-line)))
    "Face for flycheck status: warnings" :group 'flycheck)
  (defface flycheck-status-ok
    '((t (:foreground "#A0E8A0" :inherit mode-line)))
    "Face for flycheck status: OK" :group 'flycheck)
  (defface flycheck-status-error
    '((t (:foreground "#D88080" :inherit mode-line)))
    "Face for flycheck status: error" :group 'flycheck)

  (defun z-flycheck-count (s count)
    (cond ((not count) "")
          ((> count 1) (concat s (number-to-string count)))
          ((> count 0) s)
          ('t "")))

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
                         (concat
                          (propertize (z-flycheck-count "✖" .error)
                                      'face 'flycheck-status-error)
                          (when (and .error .warning) '(?/))
                          (propertize (z-flycheck-count "•" .warning)
                                      'face 'flycheck-status-warning)))
                     (propertize "✔" 'face 'flycheck-status-ok)))
                  (`running     "⁇")
                  (`no-checker  "¿")
                  (`not-checked "⁈")
                  (`errored     "‼")
                  (`interrupted "⁉")
                  (`suspicious  "‽"))))
      (list " " pick)))
(setq flycheck-mode-line '(:eval (z-flycheck-mode-line-text))))

(use-package ycmd :diminish " ☯" :no-require t
  :defines ycmd-server-command ycmd-global-config
  ycmd-extra-conf-whitelist ycmd-idle-change-delay
  :commands (ycmd-mode)
  :config
  (defconst google-ycmd--extra-conf "/usr/lib/youcompleteme/ycm_extra_conf.py")
  (setq ycmd-server-command
        '("/usr/grte/v4/bin/python2.7"
          "/usr/lib/youcompleteme/third_party/ycmd/ycmd"))
  (setq ycmd-global-config google-ycmd--extra-conf)
  (add-to-list 'ycmd-extra-conf-whitelist google-ycmd--extra-conf)
  (setq ycmd-idle-change-delay 0.5))

;; ----------------------------------------
(use-package org
  :defer 't
  :bind (("<f5>" . org-capture) ("<f6>" . org-agenda))
  :config
  (defface org-todo-open '((t :foreground "#90A8D0" :inherit org-todo))
           "face for org mode OPEN keyword" :group 'org-faces)
  (defface org-todo-wait '((t :foreground "#CCA060" :inherit org-todo))
           "face for org mode WAIT keyword" :group 'org-faces)
  (defface org-done-obsolete '((t :foreground "#909090" :inherit org-done))
           "face for org mode OBSOLETE keyword" :group 'org-faces)
  (setq org-speed-commands-user
        '(("S" . org-schedule) ("d" . org-deadline))
        org-todo-keywords
        '((sequence "TODO(t)" "OPEN(o)" "WAIT(w)"
                    "|" "OBSOLETE(e)" "DONE(d)"))
        org-todo-keyword-faces
        '(("OPEN" . org-todo-open)
          ("OBSOLETE" . org-done-obsolete) ("WAIT" . org-todo-wait))
        org-use-speed-commands 't
        org-sparse-tree-default-date-type 'closed
        org-agenda-files '("~/Projects/notes/NOTES.org")
        org-refile-targets '((nil :level . 1) (nil :tag . "OLD"))
        org-capture-templates
        '(("r" "Requests" entry
           (file+headline "~/Projects/notes/NOTES.org" "Requests")
           "** TODO %t %?")
          ("n" "Notes" entry
           (file+headline "~/Projects/notes/NOTES.org" "Captured Notes")
           "** %? %T"))
        org-link-abbrev-alist
        '(("doc" . "https://drive.google.com/drive/search?q=")
          ("ai" . "https://groups.google.com/a/google.com/forum/#!searchin/zhyang-ai/")))
  (bind-keys :map org-mode-map ("M-m") ("C-j"))
  (defun z-org-mode-hook ()
    (bug-reference-mode 1)
    (org-bullets-mode 1)
    (setq-local register-channel-move-by-default 't)
    (setq-local ido-use-filename-at-point nil))
  (add-hook 'org-mode-hook 'z-org-mode-hook))

(use-package better-shell
  :bind (("<f2>" . better-shell-shell)
         ("C-x <f2>" . better-shell-remote-open)))

;; --------------------------------------------------
;; eshell
(use-package eshell
  :commands eshell
  :functions eshell/pwd
  :config
  (defun z-eshell-prompt-function ()
    (let* ((pwd (eshell/pwd))
           (short-pwd (abbreviate-file-name pwd))
           (pwd-face 'default))
      (setq pwd-face
            (cond ((string= (nth 2 (file-attributes pwd 'string)) user-login-name)
                   'eshell-ls-symlink)
                  ((file-writable-p pwd) 'font-lock-constant-face)
                  ((file-readable-p pwd) 'eshell-ls-readonly)
                  (t 'eshell-ls-missing)))
      (concat
       (propertize short-pwd 'face pwd-face 'readonly t)
       (propertize (if (= (user-uid) 0) " # " " $ ")
                   'face 'eshell-prompt
                   'readonly t
                   'rear-nonsticky '(face readonly)))))
  (setq eshell-prompt-function 'z-eshell-prompt-function
        eshell-highlight-prompt nil
        eshell-cmpl-cycle-completions nil)

  (defalias 'eshell/x 'eshell/exit)
  (defun eshell/p (&rest args)
    "Call find-file-read-only on files"
    (mapc #'find-file-read-only
          (mapcar #'expand-file-name
                  (eshell-flatten-list (reverse args)))))

  (defun eshell/ec (&rest args)
    "Call find-file-read-only on files"
    (mapc #'find-file
          (mapcar #'expand-file-name
                  (eshell-flatten-list (reverse args)))))
  (defun z-eshell-mode-hook ()
    (company-mode 1)
    ;; somehow eshell-mode-map is buffer-local
    (bind-keys :map eshell-mode-map
             ("M-r" . counsel-esh-history)))
  (add-hook 'eshell-mode-hook 'z-eshell-mode-hook))

;; --------------------------------------------------
;; modes

(use-package elisp-mode
  :config
  (defun z-setup-imenu-for-use-package ()
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
    (z-setup-imenu-for-use-package)
    (color-identifiers-mode)
    (company-mode))
  (add-hook 'emacs-lisp-mode-hook 'z-elisp-mode-hook)
  (bind-keys :map lisp-interaction-mode-map ("C-j")))

(use-package cc-mode
  :config
  (setq c-electric-pound-behavior '(alignleft)) ;make a #define left-aligned
  (bind-key "RET" 'newline-and-indent c-mode-base-map))

(defun z-c++-mode-hook ()
  (setq flycheck-clang-language-standard "c++14"
        flycheck-gcc-language-standard "c++14")
  (abbrev-mode -1)
  (require 'clang-format nil 't))
(add-hook 'c++-mode-hook 'z-c++-mode-hook)

(use-package clang-format
  :config
  (defun z-maybe-clang-format ()
    (when (eq major-mode 'c++-mode)
      (clang-format-buffer)))

  (add-hook 'before-save-hook 'z-maybe-clang-format))

(use-package go-mode
  :config
  (defun z-go-mode-hook ()
    (setq tab-width 4)
    (setq-local company-backends '(company-ycmd))
    ;; (ycmd-mode 1)
    ;; (company-mode 1)
    ;;(go-eldoc-setup)
    )
  (add-hook 'go-mode-hook 'z-go-mode-hook))

(use-package scala2-mode
  :config
  (defun z-scala-mode-hook ()
    (setq prettify-symbols-alist
          (append '(("=>" . ?⇒)
                    ("->" . ?→))
                  prettify-symbols-alist))
    (prettify-symbols-mode))
  (add-hook 'scala-mode-hook 'z-scala-mode-hook))

(use-package haskell-mode
  :config
  (defun z-haskell-mode-hook ()
    (haskell-indentation-mode))
  ;;(setq haskell-font-lock-symbols 't)
  (add-hook 'haskell-mode-hook 'z-haskell-mode-hook))

;; --------------------------------------------------
;; ess

(use-package ess-site
  :commands (R R-mode julia)
  :mode (("\\.Rmd\\'" . R-mode))
  :defines ess-company-backends ess-current-process-name
  :functions ess-smart-S-assign ess-debug-command-next
    ess-eval-line-and-step ess-eval-linewise ess-toggle-S-assign
    ess-toggle-underscore
  :config
  (defun ess-smart-pipe ()
    "Similar to ess-smart-S-assign, but insert %>% instead."
    (interactive)
    (let ((ess-S-assign " %>% ")
          (ess-smart-S-assign-key "\\"))
      (ess-smart-S-assign)))

  (defun ess-smart-tpipe ()
    "Similar to ess-smart-S-assign, but insert %T>% instead."
    (interactive)
    (let ((ess-S-assign " %T>% ")
          (ess-smart-S-assign-key "?"))
      (ess-smart-S-assign)))

  (defun ess-debug-next-or-eval-line ()
    (interactive)
    (let ((proc (and ess-current-process-name
                     (get-process ess-current-process-name))))
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

  (defun z-ess-mode-symbols ()
    (setq prettify-symbols-alist
          (append '(("%>%" . ?↦)
                    ("%T>%" . ?↧) ;↴
                    ("%<>%" . ?⇄) ;⇋⇌⇆
                    ("<=" . ?≤)
                    (">=" . ?≥)
                    ("%in%" . ?∈)
                    ("%*%" . ?×)
                    ("function" ?ƒ))
                  prettify-symbols-alist))
    (prettify-symbols-mode))

  (defun z-ess-mode-hook ()
    (when (string-match "\\.Rmd\\'" buffer-file-name)
      (setq-local page-delimiter "^```\\({.*}\\)?$"))
    (rainbow-delimiters-mode 1)
    (z-ess-mode-symbols)
    (setq-local company-backends ess-company-backends)
    (company-mode))

  (defun z-inferior-ess-mode-hook ()
    (z-ess-mode-symbols)
    (setq-local company-backends ess-company-backends)
    (company-mode))

  (setq inferior-julia-program-name "~/bin/julia"
        ess-tab-complete-in-script 't
        ess-smart-S-assign-key ";"
        ess-busy-strings '("  " " ◴" " ◷" " ◶" " ◵"))
  (ess-toggle-S-assign nil)
  (ess-toggle-S-assign nil)
  (ess-toggle-underscore nil)

  ;; imenu recognize Rmd sections and functions. The default did not
  ;; make much sense.
  (setq ess-imenu-S-generic-expression
        '(("Section" "^\\s-*```{r \\(\\sw[a-zA-Z0-9_.]+\\)" 1)
          ("Functions" "^\\(.+\\)[      \n]*<-[         \n]*function[ ]*(" 1)))

  ;; In ESS-R, `$' is by default part of the symbol (_), which makes dabbrev
  ;; ignore variable names after $ for expansion. Fix by making it
  ;; punctuation.
  (modify-syntax-entry ?$ "." ess-r-syntax-table)

  ;; For Rmd editing, do not treat ` as quote.
  (modify-syntax-entry ?` "." ess-r-syntax-table)

  (bind-keys :map ess-mode-map
             ("<f7>" . ess-show-traceback)
             ("<f8>" . ess-debug-next-or-eval-line)
             ("<f9>" . ess-eval-function-or-paragraph-and-step)
             ("C-x <f8>" . ess-tracebug)
             ("C-c SPC" . ess-render-markdown)
             ("C-c C-m" . markdown-mode)
             ("\\" . ess-smart-pipe)
             (";" . ess-smart-S-assign))

  (bind-keys :map inferior-ess-mode-map
             ("\C-cw" . ess-execute-screen-options)
             ("<f7>" . ess-show-R-traceback)
             ("C-x <f8>" . ess-tracebug)
             ("\\" . ess-smart-pipe)
             (";" .  ess-smart-S-assign))

  (bind-keys :map ess-help-mode-map
             ("<f8>" . ess-eval-line-and-step)
             ("<f9>" . ess-eval-function-or-paragraph-and-step))

  (add-hook 'ess-mode-hook 'z-ess-mode-hook)
  (add-hook 'ess-help-mode-hook 'z-ess-mode-symbols)
  (add-hook 'inferior-ess-mode-hook 'z-inferior-ess-mode-hook))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (bind-keys :map markdown-mode-map
             ("C-c C-m" . r-mode)))

(use-package gdb-mi
  :config
  (defun z-gdb-mode-hook () (setq gdb-many-windows t))
  (add-hook 'gdb-mode-hook 'z-gdb-mode-hook))

(use-package info
  :config
  (defhydra hydra-info (:color pink :hint nil)
    "
^ ^       ^^^^Reference   ^^History       ^^Tree
_k_ ↑     _{__}_ move     _l_: back       _n_ext   _d_irectory _T_OC
_j_ ↓     ^^_f_ollow      _r_: forward    _p_rev   _<__>_ first/last
^ ^       ^^_m_enu        _L_: history    _u_p     _[__]_ back/forward
"
    ("q" Info-exit :exit t)
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
    ("{" Info-prev-reference)
    ("}" Info-next-reference)
    ("f" Info-follow-reference)
    ("l" Info-history-back)
    ("r" Info-history-forward)
    ("L" Info-history)
    ("T" Info-toc)
    ("j" Info-scroll-up)
    ("k" Info-scroll-down))
  (bind-keys :map Info-mode-map
             ("SPC" . hydra-info/body)
             ("{" . Info-prev-reference)
             ("}" . Info-next-reference)
             ("j" . Info-scroll-up)
             ("k" . Info-scroll-down)))

(use-package help-mode
  :config
  (defhydra hydra-help (:color pink :hint nil)
    "
_k_ ↑    _<_ _>_ top/bottom   _l_: back
_j_ ↓    _[_ _]_ buttons      _r_: forward
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
    ("j" scroll-up-command)
    ("[" backward-button)
    ("]" forward-button))

  (bind-keys :map help-mode-map
             ("SPC" . hydra-help/body)
             ("k" . scroll-down-command)
             ("j" . scroll-up-command)
             ("[" . backward-button)
             ("]" . forward-button)))

(use-package linum
  :config
  (defun linum-go-back-to (pos)
    "Go back to pos line by line, and return number of lines passed."
    (let ((line 0))
      (while (> (point) pos)
        (let ((inhibit-point-motion-hooks t))
          (forward-line -1))
        (setq line (1- line)))
      line))
  (defconst small-digits "₀₁₂₃₄₅₆₇₈₉")
  (defface linum-hi
    '((t :inherit linum
         :foreground "#C59F9F"
         :weight bold))
    "Linum for multiples of 10s"
    :group 'linum)
  (defun z-linum-format (line)
    (let* ((rem (mod (abs line) 10))
           (d (substring-no-properties small-digits rem (1+ rem))))
      (cond
       ((= 0 line) (propertize " " 'face 'linum))
      ((= 0 rem) (propertize (format "%d" (/ (abs line) 10)) 'face 'linum-hi))
      ((< (abs line) 10) (propertize (format "%d" rem) 'face 'linum))
      ((or (= 0 (mod rem 2)) (< (abs line) 20)) (propertize d 'face 'linum))
      ('t " "))))
  (setq linum-format 'z-linum-format)

  ;; re-define linum-update-window. The only thing that is changed is
  ;; the initial assigned value to `line'. It is now given the
  ;; relative line number from point.
  (defun linum-update-window (win)
  "Update line numbers for the portion visible in window WIN."
  (let ((line (linum-go-back-to (window-start win)))
        (limit (window-end win t))
        (fmt (cond ((stringp linum-format) linum-format)
                   ((eq linum-format 'dynamic)
                    (let ((w (length (number-to-string
                                      (count-lines (point-min) (point-max))))))
                      (concat "%" (number-to-string w) "d")))))
        (width 0))
    (goto-char (window-start win))
    (run-hooks 'linum-before-numbering-hook)
    ;; Create an overlay (or reuse an existing one) for each
    ;; line visible in this window, if necessary.
    (while (and (not (eobp)) (< (point) limit))
      (let* ((str (if fmt
                      (propertize (format fmt line) 'face 'linum)
                    (funcall linum-format line)))
             (visited (catch 'visited
                        (dolist (o (overlays-in (point) (point)))
                          (when (equal-including-properties
                                 (overlay-get o 'linum-str) str)
                            (unless (memq o linum-overlays)
                              (push o linum-overlays))
                            (setq linum-available (delq o linum-available))
                            (throw 'visited t))))))
        (setq width (max width (length str)))
        (unless visited
          (let ((ov (if (null linum-available)
                        (make-overlay (point) (point))
                      (move-overlay (pop linum-available) (point) (point)))))
            (push ov linum-overlays)
            (overlay-put ov 'before-string
                         (propertize " " 'display `((margin left-margin) ,str)))
            (overlay-put ov 'linum-str str))))
      ;; Text may contain those nasty intangible properties, but that
      ;; shouldn't prevent us from counting those lines.
      (let ((inhibit-point-motion-hooks t))
        (forward-line))
      (setq line (1+ line)))
    (when (display-graphic-p)
      (setq width (ceiling
                   (/ (* width 1.0 (linum--face-width 'linum))
                      (frame-char-width)))))
    ;; open up space in the left margin, if needed, and record that
    ;; fact as the window-parameter `linum--set-margins'
    (let ((existing-margins (window-margins win)))
      (when (> width (or (car existing-margins) 0))
        (set-window-margins win width (cdr existing-margins))
        (set-window-parameter win 'linum--set-margins (window-margins win))))))
  )


;; TODO(zhyang): use emmet-mode for html and css
