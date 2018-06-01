; -*- coding: utf-8; lexical-binding: t -*-

(eval-when-compile
  (require 'use-package)
  (require 'hydra))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
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
    ("q" quit-window :exit t)
    ("DEL" bookmark-bmenu-backup-unmark)
    ("a" bookmark-bmenu-show-annotation)
    ("A" bookmark-bmenu-show-all-annotations)
    ("e" bookmark-bmenu-edit-annotation))
  (bind-keys :map bookmark-bmenu-mode-map
             ("SPC" . hydra-bookmark-bmenu/body))
)

(use-package view :diminish view-mode
  :bind ("C-x C-v" . view-mode)         ; find-alternate-file
  :config
  (defhydra hydra-view (:color pink :hint nil)
    "
^^pg/set^^ line^^ half^^ _g_o/_%_ _{__}_ parag ^^register  ^^mark   _s_earch/_r_    _q_uit/_Q_
_k_↑ _w_   _K_    _u_p   _<_^^    _[__]_ page  _m_ark      _._set   regex: _/_ ^\\
_j_↓ _z_   _J_    _d_own _>_^^    _(__)_ list  _'_: goto   p_@_p    again: _n_ _p_
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
    ("{" backward-paragraph)
    ("}" forward-paragraph)
    ("[" backward-page)
    ("]" forward-page)
    ("(" backward-list)
    (")" forward-list)
    ("q" View-leave :color blue)
    ("Q" View-quit :color blue)
    ("C" View-kill-and-leave :color blue))
  (bind-keys :map view-mode-map
             ("SPC" . hydra-view/body)
             ("C-j" . nil)
             ("x" . god-mode-self-insert)
             ("c" . god-mode-self-insert)
             ("l" . god-mode-self-insert)
             ("{" . backward-paragraph)
             ("}" . forward-paragraph)
             ("[" . backward-page)
             ("]" . forward-page)
             ("(" . backward-list)
             (")" . forward-list)
             ("q" . View-leave)
             ("Q" . View-quit)
             ("j" . View-scroll-page-forward)
             ("k" . View-scroll-page-backward)
             ("J" . View-scroll-line-forward)
             ("K" . View-scroll-line-backward))
  (add-hook 'view-mode-hook
            (lambda () (if view-mode (god-local-mode-pause)
                         (god-local-mode-resume)))))

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
             ("SPC" . hydra-grep/body)))

(use-package dired
  :bind (("C-x C-d" . dired)
         ("C-x C-j" . dired-jump))
  :config
  (require 'dired-x)

  (defhydra hydra-dired (:color pink :columns 3 :hint nil)
    "
^Mark^‗‗‗‗‗‗‗^Flag^‗‗‗‗‗‗‗‗^Emacs Op^‗‗‗‗‗‗^‗^‗‗‗‗‗‗‗‗‗‗‗‗‗^^File Op^^‗‗(_e_dit)
_*_: marks^^   _#_: temp^^     _Q_uery/rep     _F_ind marked   _!_shell_&_ _S_ymlink
_%_: regexp^^  _~_: backup^^   _A_: grep       _L_oad          ^^_C_opy    _H_ardlink
_u_n/_m_ark    _d_: this^^     _B_yte compile  _k_ill line     ^^_D_elete  ch_M_od
_t_oggle/_U_   _x_: delete^^   _v_iew          _w_: file name  ^^_R_ename  ch_O_wn
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
             ("[" . backward-page)
             ("]" . forward-page)
             ("e" . dired-toggle-read-only)
             ("SPC" . hydra-dired/body)))

(defhydra hydra-package-menu (:color pink :hint nil)
  "
_k_↑ _p_rev    _U_pgrade      _d_elete   _f_ilter _H_ide       _r_efresh
_j_↓ _n_ext    _~_: obsolete  _i_nstall  _S_ort   _(_: toggle  _g_: revert
_<_  _>_       e_x_ecute      _?_: info  _u_nmark _q_uit
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

(use-package flymake
  :commands (flymake-mode)
  :config
  (bind-keys :map flymake-mode-map
             ("M-g `" . flymake-goto-next-error)
             ("M-g f" . flymake-goto-next-error)
             ("M-g b" . flymake-goto-prev-error)))

(use-package flycheck
  :commands (flycheck-mode)
  :config

  (defface flycheck-status-warning
    '((t (:foreground "#D8B080")))
    "Face for flycheck status: warnings" :group 'flycheck)
  (defface flycheck-status-ok
    '((t (:foreground "#98DC98")))
    "Face for flycheck status: OK" :group 'flycheck)
  (defface flycheck-status-error
    '((t (:foreground "#D88080")))
    "Face for flycheck status: error" :group 'flycheck)

  (defun z-flycheck-count (s count)
    (cond ((not count) "")
          ((> count 0) (concat s (number-to-string count)))
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
                  (`running     (propertize "✔" 'face 'flycheck-status-warning))
                  (`not-checked (propertize "✔" 'face 'flycheck-status-error))
                  (`no-checker  "¿")
                  (`errored     "‼")
                  (`interrupted "⁉")
                  (`suspicious  "‽"))))
      (list " " pick)))
  (setq flycheck-mode-line '(:eval (z-flycheck-mode-line-text))))


(use-package lsp-mode :diminish (lsp-mode . " £")
  :config
  (require 'lsp-ui-flycheck))

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
    (z-setup-imenu-for-elisp)
    ;;(company-mode)
    )
  (add-hook 'emacs-lisp-mode-hook #'z-elisp-mode-hook)
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
(add-hook 'c++-mode-hook #'z-c++-mode-hook)

(use-package clang-format
  :config
  (defun z-maybe-clang-format ()
    (when (eq major-mode 'c++-mode)
      (clang-format-buffer)))

  (add-hook 'before-save-hook #'z-maybe-clang-format))

(use-package go-mode
  :config
  (defun z-go-mode-hook ()
    (setq tab-width 4)
    (flycheck-mode)
    (flycheck-select-checker 'go-gofmt)
    ;; (setq-local company-backends '(company-ycmd))
    ;; (ycmd-mode 1)
    ;; (company-mode 1)
    ;;(go-eldoc-setup)
    )
  (add-hook 'go-mode-hook #'z-go-mode-hook))

(use-package js
  :config
  (bind-keys :map js-mode-map
             ("M-." . nil)
             ("C-x ." . js-find-symbol)))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-strict-missing-semi-warning nil))

(use-package haskell-mode
  :config
  (defun z-haskell-mode-hook ()
    (haskell-indentation-mode))
  ;;(setq haskell-font-lock-symbols 't)
  (add-hook 'haskell-mode-hook #'z-haskell-mode-hook))

;; --------------------------------------------------
;; ess

(use-package ess-site
  :commands (R R-mode julia)
  :bind ("<f5>" . z-switch-to-R)
  :mode (("\\.Rmd\\'" . R-mode))
  :defines ess-company-backends ess-current-process-name
  :functions ess-smart-S-assign ess-debug-command-next
    ess-eval-line-and-step ess-eval-linewise ess-toggle-S-assign
    ess-toggle-underscore
  :config
  (defun z-switch-to-R ()
    "Go to R session or create one if none exists"
    (interactive)
    (let ((b (or (get-buffer "*R*")
                (cl-find-if (lambda (b)
                              (with-current-buffer b
                                (eq major-mode 'inferior-ess-mode)))
                            (buffer-list)))))
      (if b (switch-to-buffer b)
        (let ((ess-ask-for-ess-directory nil)
              (ess-directory "~/Projects"))
          (R)))))

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
                    ("!=" . ?≠)
                    ("%in%" . ?∈)
                    ("%*%" . ?×)
                    ("function" . ?ƒ))
                  prettify-symbols-alist))
    (prettify-symbols-mode))

  (defun z-ess-mode-hook ()
    (when (string-match "\\.Rmd\\'" buffer-file-name)
      (setq-local page-delimiter "^```\\({.*}\\)?$"))
    (rainbow-delimiters-mode 1)
    (z-ess-mode-symbols)
    ;; (setq-local company-backends ess-company-backends)
    ;; (company-mode)
    )

  (defun z-inferior-ess-mode-hook ()
    (z-ess-mode-symbols)
    (setq-local scroll-margin 0)
    (setq-local comint-move-point-for-output t)
    ;; (setq-local company-backends ess-company-backends)
    ;; (company-mode)
    )

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

  (add-hook 'ess-mode-hook #'z-ess-mode-hook)
  (add-hook 'ess-help-mode-hook #'z-ess-mode-symbols)
  (add-hook 'inferior-ess-mode-hook #'z-inferior-ess-mode-hook))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-header-scaling 't)
  :config
  (bind-keys :map markdown-mode-map
             ("C-c C-m" . r-mode)))

(use-package gdb-mi
  :config
  (defun z-gdb-mode-hook () (setq gdb-many-windows t))
  (add-hook 'gdb-mode-hook #'z-gdb-mode-hook))

(use-package man
  :config
  (defhydra hydra-man (:color pink :hint nil)
    "
_k_↑ _<__>_ top/bottom  _p_rev sec   _g_oto sec  _m_an        _K_ill
_j_↓ _[__]_ button      _n_ext sec   _s_ee also  _r_eference  _q_uit
"
    ("SPC" nil :exit t)
    ("j" scroll-up-command)
    ("k" scroll-down-command)
    ("K" Man-kill :exit t)
    ("q" quit-window :exit t)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("[" backward-button)
    ("]" forward-button)
    ("n" Man-next-section)
    ("p" Man-prev-section)
    ("g" Man-goto-section)
    ("s" Man-goto-see-also-section)
    ("m" man)
    ("r" Man-follow-manual-reference))
  (bind-keys :map Man-mode-map
             ("SPC" . hydra-man/body)
             ("j" . scroll-up-command)
             ("k" . scroll-down-command)
             ("K" . Man-kill)
             ("[" . backward-button)
             ("]" . forward-button)
             ("x" . god-mode-self-insert)))

(use-package info
  :config
  (defhydra hydra-info (:color pink :hint nil)
    "
^ ^      ^^^^Reference   ^^History       ^^Tree
_k_↑     _{__}_ move     _l_: back       _n_ext   _d_irectory _T_OC
_j_↓     ^^_f_ollow      _r_: forward    _p_rev   _<__>_ first/last
^ ^      ^^_m_enu        _L_: history    _u_p     _[__]_ back/forward
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
             ("k" . Info-scroll-down)
             ("x" . god-mode-self-insert)))

(use-package help-mode
  :config
  (defhydra hydra-help (:color pink :hint nil)
    "
_k_↑    _<_ _>_ top/bottom   _l_: back
_j_↓    _[_ _]_ buttons      _r_: forward
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
             ("]" . forward-button)
             ("x" . god-mode-self-insert)
             ("<mouse-8>" . help-go-back)
             ("<mouse-9>" . help-go-forward)))

(use-package server :diminish (server-buffer-clients . " #")
  :config (add-hook 'after-init-hook 'server-start))

(use-package edit-server :ensure
  :diminish (edit-server-edit-mode . " ✆")
  :config
  (setq edit-server-new-frame nil
        edit-server-url-major-mode-alist
        '(("mail\\.google\\.com" . html-mode)
          ("snippets\\.googleplex\\.com" . markdown-mode)))
  (add-hook 'after-init-hook 'edit-server-start))

(use-package shell
  :config
  (bind-keys :map shell-mode-map
             ("C-c C-l" . counsel-shell-history)))
