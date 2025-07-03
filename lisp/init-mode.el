; -*- coding: utf-8; lexical-binding: t -*-

(eval-when-compile
  (require 'use-package)
  (require 'keymap-hint))

(use-package keymap-hint
  :commands (keymap-hint-set keymap-hint-show))

(use-package bookmark
  :config
  (keymap-hint-set bookmark-bmenu-mode-map "
_m_ark╶──··──────╮ Item╶─····────··────────┬╴Annot··╶╮ List··╶────╮
_⏎_:go   _u_nmark│ _v_isit··     _r_ename    _a_:show│ _s_ave     │
_d_elete _⌫_:back│ _1_/_2_-win   _w_here     _A_ll   │ _l_oad     │
_z_ap    ··      │ _o_ther-win·· _R_elocate  _e_dit  │ _t_gl Fname│
" :bind "SPC")
  (bind-keys :map bookmark-bmenu-mode-map
             ("j" . scroll-down-command)
             ("k" . scroll-up-command)
             ("z" . bookmark-bmenu-execute-deletions)
             ("x" . god-mode-self-insert))
  (put 'bookmark-bmenu-list 'command-semantic 'switch-to-buffer))

(use-package view :diminish view-mode
  :bind ("C-x C-v" . view-mode)         ; find-alternate-file
  :config
  (keymap-hint-set view-mode-map "
··pg/set·· ½··╶──╮ _g_o(_%_)····╶─────╮ ··Register┬╴··Mark╮ _s_earch/_r_·╶╮ _q_uit/_Q_
_k_↥ _K_   _u_p  │ _{__}_  _[__]_ page│ _m_:point   _._set│ again:_S_ _R_·│ _i_menu
_j_↧ _J_   _d_own│ _<__>_  _(__)_ list│ _'_:goto    p_@_p │ regex:_/_ _\\_│ _o_utline
" :bind "SPC")

  (bind-keys :map view-mode-map
             ("C-j" . nil)
             ("(" . backward-list)
             (")" . forward-list)
             ("[" . backward-page)
             ("]" . forward-page)
             ("{" . backward-paragraph)
             ("}" . forward-paragraph)
             ("a" . move-beginning-of-line)
             ("b" . backward-char)
             ("c" . god-mode-self-insert) ;View-leave
             ("e" . move-end-of-line)
             ("f" . forward-char)
             ("g" . consult-goto-line)  ;View-goto-line
             ("h" . god-mode-self-insert)
             ("i" . consult-imenu)
             ("j" . View-scroll-page-forward)
             ("k" . View-scroll-page-backward)
             ("J" . View-scroll-page-forward-set-page-size)
             ("K" . View-scroll-page-backward-set-page-size)
             ("l" . recenter-top-bottom)
             ("n" . next-line)
             ("o" . consult-outline)
             ("p" . previous-line)
             ("q" . View-exit)
             ("Q" . View-leave)
             ("R" . View-search-last-regexp-backward)
             ("S" . View-search-last-regexp-forward)
             ("v" . scroll-up-command)
             ("x" . god-mode-self-insert) ;exchange-point-and-mark
             ("z" . repeat)
             ;;("C-x k" . View-kill-and-leave)
             )

  (put 'View-scroll-page-forward 'command-semantic 'scroll-up-command)
  (put 'View-scroll-page-backward 'command-semantic 'scroll-down-command)

  (add-hook 'view-mode-hook
            (lambda () (if view-mode (god-local-mode-pause)
                         (god-local-mode-resume)))))

;; (use-package rg
;;   :bind (("M-s r" . rg)
;;          ("M-s R" . rg-menu)))

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
             ("M-e"   . consult-isearch-history) ;isearch-edit-string
             ("M-k"   . isearch-yank-word-or-char)
             ("M-z"   . isearch-yank-until-char)
             ("M-<"   . isearch-beginning-of-buffer)
             ("M->"   . isearch-end-of-buffer)
             ("C-j"   . avy-isearch))

  (setq isearch-allow-motion 't)
  (setcdr (assq 'isearch-mode minor-mode-alist)
          '((:eval (if isearch-forward " »" " «")))))

(use-package format-expand
  :bind ("M-s f" . format-expand))

(use-package xref
  :config
  (bind-keys :map xref--xref-buffer-mode-map
             ("[" . xref-prev-group)
             ("]" . xref-next-group)))

(use-package replace
  :bind ("M-s M-o" . multi-occur-in-matching-buffers)
  :config
  (keymap-hint-set
   occur-mode-map
   (format "
_k_↥  _p_rev   _<_ _>_       _⏎_:goto      %s _e_dit
_j_↧  _n_ext   _d_isplay··   _o_ther-win   %s _^c^f_ollow"
           (mode-char 'occur-edit-mode)
           (mode-char 'next-error-follow-minor-mode))
   :bind "SPC")
  (bind-keys :map occur-mode-map
             ("SPC" . occur-mode-map-hint)
             ("d" . occur-mode-display-occurrence)
             ("j" . scroll-up-command)
             ("k" . scroll-down-command)
             ("n" . occur-next)
             ("p" . occur-prev)
             ("x" . god-mode-self-insert)
             ("c" . god-mode-self-insert))

  ;; For lighter hint
  (put 'occur-next 'command-semantic 'next-line)
  (put 'occur-prev 'command-semantic 'previous-line))

(use-package grep
  :bind (("M-s g" . grep)
         ("M-s G" . rgrep))
  :config
  (setq grep-use-headings 't)

  (keymap-hint-set grep-mode-map "
_k_↥  _p_rev  _<__>_ beg/end of buffer _⏎_:goto    _e_dit
_j_↧  _n_ext  _[__]_:prev/next file    _d_isplay
" :bind "SPC")

  (bind-keys :map grep-mode-map
             ("j" . scroll-up-command)
             ("k" . scroll-down-command)
             ("d" . compilation-display-error)
             ("e" . grep-change-to-grep-edit-mode) ;compatibility for 29
             ("[" . compilation-previous-file)
             ("]" . compilation-next-file)
             ("x" . god-mode-self-insert)
             ("c" . god-mode-self-insert))
  (unless (fboundp #'grep-change-to-grep-edit-mode)
    (defalias 'grep-change-to-grep-edit-mode 'wgrep-change-to-wgrep-mode)
    (autoload 'wgrep-change-to-wgrep-mode "wgrep"))
  (put 'grep 'command-semantic 'display-buffer)
  (put 'rgrep 'command-semantic 'display-buffer))

(use-package compile
  :config
  (keymap-hint-set compilation-mode-map "
_k_↥  _p_rev  _<__>_ beg/end of buffer  _⏎_:goto
_j_↧  _n_ext  _[__]_:prev/next file
" :bind "SPC")
  (bind-keys :map compilation-mode-map
             ("x" . god-mode-self-insert)
             ("c" . god-mode-self-insert)
             ("`" . next-error)
             ("j" . scroll-up-command)
             ("k" . scroll-down-command)
             ("n" . compilation-next-error)
             ("p" . compilation-previous-error)
             ("[" . compilation-previous-file)
             ("]" . compilation-next-file))

  (put 'compilation-next-error 'command-semantic 'next-line)
  (put 'compilation-previous-error 'command-semantic 'previous-line))

(use-package dired
  :bind (("C-x C-d" . dired)
         ("C-x C-j" . dired-jump)
         ("C-x 4 j" . dired-jump-other-window))
  :config
  (setq dired-dwim-target 't)

  (keymap-hint-set dired-mode-map "
··Mark(_*_)╶╮ ·Flag·╶────··──────╮ Go··╶(_j_ump)─╮ Dir··╶────··──────┬ Subdir··╶╮
_%_:regexp··│ _#_:temp   _d_:this│ _[__]_:page   │ _s_ort    _(_ detl  _i_nsert │
_u_n/_m_ark │ _~_:backup _z_ap   │ _<__>_:dirline│ _^_ up    ··        _$_:hide │
_t_oggle/_U_│ _._:№ bkup   ··    │ _{__}_:marked │ _+_create ··        _K_ill   │

·Emacs Ops···╶─────────────··─────────╮ ··File Ops··╶─(_e_dit)··──··───────┬ch╶··╮
_F_ind all   file-t_y_pe   _v_iew     │ _!_shell_&_ _S__Y_mlink  _=_ diff   _M_od│
_A_:grep     _w_:cp Fname  _a_ltern   │ ··_C_opy    _H_ardlink·· _c_ompress _O_wn│
_Q_uery/rep  _k_ill-line   _o_ther-win│ ··_D_elete  _T_ouch··    _Z_ompress _G_rp│
_B_yte comp  _I_nfo        ··         │ ··_R_ename  _P_rint··    _W_eb      ··   │
_L_oad       ma_N_         redisp_l_ay│ ··_E_xt-open ····        ··         ··   │
" :bind "SPC")

  (keymap-hint-set-sub dired-mode-map "*" "
_m_ark╶────────··─────────··──────╮ _u_nmark╶╮ _t_oggle
_*_executable _/_dir      _s_ubdir│ _!_all   │ _c_hange
_%_regexp     _@_symlink  ··      │ _?_char  │ _N_umber
" :bind "SPC")

  (keymap-hint-set-sub dired-mode-map "%" "
Flag╶─··──╮ _m_ark╶────╮ _r_ename╶──╮ _C_opy
_&_garbage│ _g_rep-mark│ _u_pcase   │ _H_ardlink
_d_:this  │ ··         │ _l_owercase│ _S__Y_mlink
" :bind "SPC")

  (bind-keys :map dired-mode-map
             ("[" . backward-page)
             ("]" . forward-page)
             ("{" . dired-prev-marked-file)
             ("}" . dired-next-marked-file)
             ("z" . dired-do-flagged-delete)
             ("x" . god-mode-self-insert)
             ("e" . dired-toggle-read-only)
             ("K" . dired-kill-subdir))
  (put 'dired-next-line 'command-semantic 'next-line)
  (put 'dired-previous-line 'command-semantic 'previous-line)
  (put 'dired-find-file 'command-semantic 'find-file)
  (put 'dired-up-directory 'command-semantic 'display-buffer)
  (put 'dired 'command-semantic 'find-file)
  (put 'dired-jump 'command-semantic 'find-file)
  (put 'dired-jump-other-window 'command-semantic 'display-buffer))

(use-package package
  :init
  ;; Make elpa packages available
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (setq package-archive-priorities
        '(("melpa-stable" . 20) ("gnu" . 10) ("melpa" . 0)))

  :config
  (keymap-hint-set package-menu-mode-map "
Go····╶────╮ Action_z_╶─··────────────··──────╮ List··╶─··──────··──────╮
_k_↥ _p_rev│ _i_nstall  _d_elete      _?_:info│ _/_:filter··    _r_evert│
_j_↧ _n_ext│ _U_pgrade  _~_:obsolete  _u_nmark│ _H_ide/_(_tgl)  _S_ort  │
" :bind "SPC")

  (keymap-hint-set-sub package-menu-mode-map "/" "
_/_clear      _a_rchive  _d_escription  _k_eyword  _m_arked  _n_ame  _s_tatus
_u_pgradable  _v_ersion  _N_ame/desc
" :bind "SPC")
  (bind-keys :map package-menu-mode-map
             ("z" . package-menu-execute)
             ("x" . god-mode-self-insert)
             ("a" . move-beginning-of-line)
             ("e" . move-end-of-line)
             ("s" . consult-line)
             ("j" . scroll-up-command)
             ("k" . scroll-down-command))
  (put 'list-packages 'command-semantic 'display-buffer))

(use-package smerge-mode
  :bind ("C-x m" . smerge-mode)
  :config
  (keymap-hint-set smerge-mode-map "
Move··╶╮ Keep··╶─··─────╮ Current··╮ Conflict····──────┬╴diff··╶─────────╮
_n_ext │ _b_ase  _u_pper│ _⏎_ keep │ _R_efine  _E_diff   _<_: base-upper │
_p_rev │ _a_ll   _l_ower│ _K_ill   │ _C_ombine/a_U_to    _=_: upper-lower│
_q_uit │ ··      _s_wap │ ··       │ _r_esolve/_A_ll     _>_: base-lower │
" :bind "SPC")
)

(use-package diff-mode
  :config
  ;; diff-mode-map put diff-mode-shared-map on its ESC binding. The
  ;; latter inherits special-mode-map, for which I added a bunch of
  ;; stuff that needs to be masked.
  (bind-keys :map diff-mode-map
             ("M--") ("M-9") ("M-0") ("M-SPC")
             ("M-l") ("M-o") ("M-v") ("M-x")))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq with-editor-mode-lighter "")

  (bind-keys :map magit-mode-map
             ("[" . magit-section-backward-sibling)
             ("]" . magit-section-forward-sibling)
             ("x" . god-mode-self-insert))
  (put 'magit-section-forward 'command-semantic 'next-line)
  (put 'magit-section-backward 'command-semantic 'previous-line))

(use-package eldoc :diminish eldoc-mode
  :commands eldoc-mode)

(use-package bug-reference
  :commands bug-reference-mode
  :config
  (defun z-bug-to-link ()
    "Convert text captured from bug-reference-bug-regexp into links."
    (let ((m (match-string 2)))
      (if (string-suffix-p "@" m)
          (concat "teams/" (string-trim-right m "@"))
        (concat "http://" m))))
  (put 'z-bug-to-link 'bug-reference-url-format 't)
  (setq bug-reference-url-format 'z-bug-to-link
        bug-reference-bug-regexp
        "\\(?:\\b\\)\\(b/[0-9]+\\|c[rl]/[0-9]+\\|t/[0-9]+\\|\\(?:g\\|go\\|goto\\)/[-a-zA-z0-9_]+\\|[a-z]+@\\)"))

(use-package rainbow-identifiers
  :commands (rainbow-identifiers-mode)
  :config
  (setq rainbow-identifiers-choose-face-function
        'rainbow-identifiers-cie-l*a*b*-choose-face
        rainbow-identifiers-cie-l*a*b*-lightness 85
        rainbow-identifiers-cie-l*a*b*-saturation 8))

(use-package outline
  :config
  (bind-keys :map outline-mode-map
             ("C-M-a" . outline-previous-visible-heading)
             ("C-M-e" . outline-next-visible-heading)))

;; --------------------------------------------------
(use-package yasnippet
  :defer 5
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
  "Search RE backward, return COUNT submatch.  Used in snippets.

Limit search to a few pages before."
  (save-excursion
    (save-match-data
      (when (re-search-backward
             re
             (max (point-min)
                  (- (window-start) 5000)) t)
        (match-string count)))))

(use-package flymake
  :commands (flymake-mode)
  :config

  (setq flymake-mode-line-lighter "")

  (bind-keys :map flymake-mode-map
             ("M-g k"   . consult-flymake)
             ("M-g M-k" . flymake-show-buffer-diagnostics)
             ("M-g f"   . flymake-goto-next-error)
             ("M-g b"   . flymake-goto-prev-error)))

(use-package completion-preview
  :diminish " ©"
  :config
  (setq completion-preview-completion-styles '(orderless-first-prefix))
  (bind-keys :map completion-preview-active-mode-map
             ("M-i")                    ; conflicts with goto-chg
             ;; consult capf seems to mess up the preview when completing
             ("C-M-i" . completion-preview-complete)
             ("M-n" . completion-preview-next-candidate)
             ("M-p" . completion-preview-prev-candidate)
             ([remap forward-word] . completion-preview-insert-word)
             ([remap forward-sexp] . completion-preview-insert-sexp)))

;; --------------------------------------------------
;;; modes

(use-package elisp-mode
  :config
  (defun z-setup-imenu-for-elisp ()
    "Recognize `use-package` in imenu, for init files."
    (let ((emacsd (expand-file-name "~/.emacs.d/lisp/"))
          (initel (expand-file-name "init.el" "~/.emacs.d")))
      (when (and buffer-file-name
                 (or (string= buffer-file-name initel)
                     (string-match (rx-to-string `(: bos ,emacsd) t)
                                   buffer-file-name)))
        (add-to-list
         'imenu-generic-expression
         '("Packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)))))

  (setq emacs-lisp-directory
        (replace-regexp-in-string "/lisp/.*" "" (symbol-file 'elisp-mode)))

  ;; see use-package-core.el; somehow I still need this for emacs 30
  (font-lock-add-keywords 'emacs-lisp-mode use-package-font-lock-keywords)

  (defun z-elisp-mode-hook ()
    (z-setup-imenu-for-elisp)
    ; turn on read-only mode for emacs bundled elisp files
    (when (string-prefix-p emacs-lisp-directory
                          (buffer-file-name))
      (read-only-mode 1))
    (completion-preview-mode 1))
  (add-hook 'emacs-lisp-mode-hook #'z-elisp-mode-hook)
  (bind-keys :map emacs-lisp-mode-map
             ("M-L" . string-inflection-kebab-case))
  (bind-keys :map lisp-interaction-mode-map
             ("C-j")
             ("M-L" . string-inflection-kebab-case)))

(use-package cc-mode
  :config
  (setq c-electric-pound-behavior '(alignleft)) ;make a #define left-aligned
  (keymap-set c-mode-base-map "RET" #'newline-and-indent)

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

;; (use-package js
;;   :config
;;   (bind-keys :map js-mode-map
;;              ("M-." . nil)              ; not needed in 30
;;              ([remap xref-find-definitions] . js-find-symbol)))

(use-package julia-repl
  :hook (julia-mode . julia-repl-mode)
  :config
  (setq julia-repl-captures
        (list (kbd "M-x") (kbd "<home>")
              (kbd "M-9") (kbd "M-0") (kbd "M-o"))))

(use-package pico8-mode
  :config
  (defun z-pico8-mode-hook ()
    (setq-local lua-indent-level 1))
  (add-hook 'pico8-mode-hook #'z-pico8-mode-hook))

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
    (let ((b (cl-find-if
              ;; is there another buffer to switch to?
              (lambda (b)
                (and (provided-mode-derived-p
                      (buffer-local-value 'major-mode b)
                      '(inferior-ess-mode inferior-julia-mode))
                     (not (eq b (current-buffer)))))
              (buffer-list))))
      (if b (switch-to-buffer b)
        (unless (provided-mode-derived-p major-mode 'inferior-ess-mode)
          (let ((ess-ask-for-ess-directory nil)
                (ess-startup-directory "~/Projects"))
            (run-ess-r))))))

  (defun ess-smart-pipe (arg)
    "Similar to `ess-insert-assign', but insert |> instead."
    (interactive "p")
    (let ((ess-assign-list '(" |> ")))
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
  ;; ess forgot this
  (put 'ess-help-mode 'mode-class 'special)

  (bind-keys :map ess-help-mode-map
             ("<f8>" . ess-eval-line-and-step)
             ("<f9>" . ess-eval-function-or-paragraph-and-step)
             ("j" . scroll-up-command)
             ("k" . scroll-down-command)
             ("[" . ess-skip-to-previous-section)
             ("]" . ess-skip-to-next-section)
             ("n" . next-line)
             ("p" . previous-line)
             ("x" . god-mode-self-insert)
             ("c" . god-mode-self-insert)
             ("a" . move-beginning-of-line)
             ;; e already bound to move-end-of-line
             )

  (keymap-hint-set ess-help-mode-map "
Move···╶─────·····─────────╮ Eval····╶──────╮ Jump··╶────────··────────╮
_k_↥ _p_rev  _[__]_:section│ _f_unction ··  │ _h_elp-on-obj  _/_isearch│
_j_↧ _n_ext  _<__>_:buf    │ _l_ine _r_egion│ _w_eb          _i_ndex   │
" :bind "SPC")
)


(use-package ess-r-mode
  :mode (("\\.Rmd\\'" . ess-r-mode))
  :config
  ;; For Rmd editing, do not treat ` as quote.
  (modify-syntax-entry ?` "." ess-r-mode-syntax-table)
  (modify-syntax-entry ?% "." ess-r-mode-syntax-table)

  (setq ess-r-prettify-symbols
        '(("%>%" . ?↦)                  ; magrittr pipe
          ("|>" . ?▷)                   ; R pipe
          ("\\" . ?λ)
          ("<-" . ?←)
          ("->" . ?→)
          ("<=" . ?≤)
          (">=" . ?≥)
          ("!=" . ?≠)
          ("%in%" . ?∈)
          ("%*%" . ?×)
          ;;("function" . ?ƒ)
          ))
  (setenv "ESS_BACKGROUND_MODE" "dark")
  (setenv "R_CLI_NUM_COLORS" "256")

  (defun z-inferior-ess-mode-hook ()
    (setq prettify-symbols-alist ess-r-prettify-symbols)
    (prettify-symbols-mode 1)
    (setq-local scroll-margin 0)
    (setq-local comint-move-point-for-output t))
  (add-hook 'inferior-ess-r-mode-hook #'z-inferior-ess-mode-hook)

  (bind-keys :map ess-r-mode-map
             ;; normally bound to C-RET, which is awkward to press
             ("M-RET" . ess-eval-region-or-line-visibly-and-step))

  ;; TODO: map ess-r-help-mode-map "s" key

  (bind-keys :map inferior-ess-r-mode-map
             ("\C-cw" . ess-execute-screen-options)
             ("<f7>" . ess-show-R-traceback)
             ("C-x <f8>" . ess-tracebug)
             ("_")
             ("C-M-a" . comint-previous-prompt)
             ("C-M-e" . comint-next-prompt)
             ("\\" . ess-smart-pipe)
             (";" .  ess-cycle-assign)))

(use-package text-mode
  :config
  (bind-keys :map text-mode-map
             ("C-M-a" . backward-paragraph)
             ("C-M-e" . forward-paragraph)))

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

(use-package tex-mode
  :config
  (bind-keys :map tex-mode-map
             ("C-j"))                   ; was on tex-handle-newline
  (add-hook 'tex-mode-hook
            (lambda ()
              (prettify-symbols-mode 1))))

(use-package mhtml-mode
  :config
  (bind-keys :map mhtml-mode-map ("M-o"))
  (defun z-mhtml-mode-hook ()
    (sgml-electric-tag-pair-mode 1))
  (add-hook 'mhtml-mode-hook #'z-mhtml-mode-hook))

;; ----------------------------------------------------------
(use-package gdb-mi
  :config
  (defun z-gdb-mode-hook () (setq gdb-many-windows t))
  (add-hook 'gdb-mode-hook #'z-gdb-mode-hook))

(use-package man
  :config
  (put 'Man-mode 'mode-class 'special)
  (keymap-hint-set Man-mode-map "
_k_↥ _<__>_  top/bot  _g_oto sec··    _r_eference │ _K_ill
_j_↧ _⇧_/_⇥_ button   _[__]_ section  _s_ee also  │ _q_uit
" :bind "SPC")
  (bind-keys :map Man-mode-map
             ("j" . scroll-up-command)
             ("k" . scroll-down-command)
             ("n" . next-line)
             ("p" . previous-line)
             ("K" . Man-kill)
             ("[" . Man-previous-section)
             ("]" . Man-next-section)
             ("{" . backward-paragraph)
             ("}" . forward-paragraph)
             ("x" . god-mode-self-insert)))

(use-package info
  :config
  (keymap-hint-set Info-mode-map "
Go··╶╮ ··Reference··╶╮ ··History╶─╮ Tree······╶─┬ _d_irectory··───╮
_k_↥ │ _⇧_/_⇥_:cycle │ _l_:back   │ ··  ↑_u_p·· │ In file:_T_OC·· │
_j_↧ │ _⏎_:_f_ollow  │ _r_:forward│ _P_←∙→_N_·· │ _<__>_first/last│
· ·  │ _m_enu··      │ _L_ist     │ ··  ↳_[__]_ │ ····            │
" :bind "SPC")
  (bind-keys :map Info-mode-map
             ("e" . move-end-of-line)
             ("j" . Info-scroll-up)
             ("k" . Info-scroll-down)
             ("n" . next-line)
             ("p" . previous-line)
             ("N" . Info-next)
             ("P" . Info-prev)
             ("M-n")                    ; this was clone-buffer
             ("M-g i" . Info-menu)
             ("{" . backward-paragraph)
             ("}" . forward-paragraph)
             ("x" . god-mode-self-insert))

  (put 'Info-scroll-up 'command-semantic 'scroll-up-command)
  (put 'Info-scroll-down 'command-semantic 'scroll-down-command))

(use-package help-mode
  :config
  (keymap-hint-set help-mode-map "
_k_↥  _l_/_r_:history  _i_nfo       _s_ource
_j_↧  _⇧_/_⇥_:buttons  _I_:lispref  _c_ustomize
" :bind "SPC")

  (bind-keys :map help-mode-map
             ("n" . next-line)
             ("p" . previous-line)
             ("[" . help-goto-previous-page)
             ("]" . help-goto-next-page)
             ("x" . god-mode-self-insert))
  ;; Handle keymap in C-h o. We copied the whole thing from the
  ;; default definition to remove keymap from `describe-variable'.
  (setq describe-symbol-backends
        `(("keymap"
           ,(lambda (sym) (and (boundp sym) (keymapp (symbol-value sym))))
           ,(lambda (s _b _f) (describe-keymap s)))
          ("function" ,#'fboundp ,(lambda (s _b _f) (describe-function s)))
          ("variable"
           ,(lambda (symbol)
              (unless (and (boundp symbol)
                           (keymapp (symbol-value symbol)))
                (or (and (boundp symbol) (not (keywordp symbol)))
                    (get symbol 'variable-documentation))))
           ,#'describe-variable)
          ;; FIXME: We could go crazy and add another entry so describe-symbol can be
          ;; used with the slot names of CL structs (and/or EIEIO objects).
          ("type" ,#'cl-find-class ,#'cl-describe-type)
          ("face" ,#'facep ,(lambda (s _b _f) (describe-face s)))))
  )

(use-package server :diminish (server-buffer-clients . " #")
  :init (add-hook 'after-init-hook 'server-start)
  ;; :config
  ;; (setq server-use-tcp 't)
  ;; (setq server-port 9527)
  )

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

(use-package gap-process
  :config
  (setq gap-executable "/opt/homebrew/bin/gap"))

(use-package comint
  :config
  (ansi-color-for-comint-mode-on)
  (setq comint-scroll-to-bottom-on-output 't
        comint-scroll-show-maximum-output nil))

(use-package tramp-sh
  :config
  ;; does not work if this is loaded directly. Delay this a bit
  (run-with-timer
   10 nil
   (lambda ()
     (setq tramp-use-ssh-controlmaster-options nil)
     (setq tramp-use-connection-share nil)))
)

;(use-package eterm-256color
;  :hook (term-mode . eterm-256color-mode))

(provide 'init-mode)
;;; init-mode.el ends here
