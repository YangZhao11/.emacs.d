(setq auto-mode-alist
      (append
       '(("NOTES$" . org-mode)
         ("\\.Rmd\\'" . markdown-mode))
       auto-mode-alist))

;; org mode link abbreviations
(setq org-link-abbrev-alist
      '(("wiki" . "http://wiki/Main/")
        ("google3" . "http://cs/#piper///depot/google3/")
        ("google" . "http://www.google.com/search?ie=UTF-8&q=")
        ("go" . "http://go/")))

;; --------------------------------------------------
;; eshell
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
      eshell-highlight-prompt nil)

;; --------------------------------------------------
;; modes
(defun z-emacs-lisp-mode-hook () (rainbow-delimiters-mode))
(add-hook 'emacs-lisp-mode-hook 'z-emacs-lisp-mode-hook)

(defun z-c-mode-common-hook ()
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map (kbd "C-x t d") 'doxymacs-mode)
  (define-key c-mode-base-map (kbd "C-x t m") 'cwarn-mode)
  (setq c-electric-pound-behavior '(alignleft)) ;make a #define left-aligned
  (whitespace-mode 1)
)
(add-hook 'c-mode-common-hook 'z-c-mode-common-hook)
(add-hook 'text-mode-hook 'auto-fill-mode)

(defun z-go-mode-hook ()
  (setq tab-width 4))
(add-hook 'go-mode-hook 'z-go-mode-hook)


(defun z-scala-mode-hook ()
  (when (fboundp 'prettify-symbols-mode) ; 24.4 needed
    (setq prettify-symbols-alist
          (append '(("=>" . ?⇒)
                    ("->" . ?→))
                  prettify-symbols-alist))
    (prettify-symbols-mode)))
(add-hook 'scala-mode-hook 'z-scala-mode-hook)

;; --------------------------------------------------
;; ess
(require 'ess-site)
(setq inferior-julia-program-name "~/bin/julia")
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
  (let ((proc (get-process ess-current-process-name) ))
    (if (not (or (process-get proc 'dbg-active)
                 (process-get proc 'is-recover)))
        (ess-eval-line-and-step)
      (ess-debug-command-next))))

(defun z-ess-mode-symbols ()
  (when (fboundp 'prettify-symbols-mode) ; 24.4 needed
    (setq prettify-symbols-alist
          (append '(("%>%" . ?↦)
                    ("%T>%" . ?↧) ;↴
                    ("%<>%" . ?⇄) ;⇋⇌⇆
                    ;("%$%" . ?⊙)  ;⊕
                    ("<=" . ?≤)
                    (">=" . ?≥)
                    ("%in%" . ?∈)
                    ("%*%" . ?×)
                    ("function" ?ƒ))
                  prettify-symbols-alist))
    (prettify-symbols-mode)))

(defun z-ess-mode-hook ()
  (define-key ess-mode-map [f7] 'ess-show-traceback)
  (define-key ess-mode-map [f8] 'ess-debug-next-or-eval-line)
  (define-key ess-mode-map [f9]
    'ess-eval-function-or-paragraph-and-step)
  (define-key ess-mode-map (kbd "C-x <f8>") 'ess-tracebug)
  (define-key ess-mode-map (kbd "\\") 'ess-smart-pipe)
  (define-key ess-mode-map (kbd "?") 'ess-smart-tpipe)
  (setq ess-tab-complete-in-script t)
  (whitespace-mode 1)
  (z-ess-mode-symbols))
(add-hook 'ess-mode-hook 'z-ess-mode-hook)

(defun z-ess-help-mode-hook ()
  (define-key ess-help-mode-map [f8] 'ess-eval-line-and-step)
  (define-key ess-help-mode-map [f9]
    'ess-eval-function-or-paragraph-and-step)
  (z-ess-mode-symbols))
(add-hook 'ess-help-mode-hook 'z-ess-help-mode-hook)
(defun z-inferior-ess-mode-hook ()
  (define-key inferior-ess-mode-map "\C-cw"
    'ess-execute-screen-options)
  (define-key inferior-ess-mode-map [f7] 'ess-show-R-traceback)
  (define-key inferior-ess-mode-map (kbd "C-x <f8>") 'ess-tracebug)
  (define-key inferior-ess-mode-map (kbd "\\") 'ess-smart-pipe)
  (z-ess-mode-symbols))
(add-hook 'inferior-ess-mode-hook 'z-inferior-ess-mode-hook)

;; Remove ## as begining of comment. Google R style guide insists we use
;; single #.
(defun z-remove-fancy-comments ()
  (interactive)
  (when (eq major-mode 'ess-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\( *\\)## " nil t)
        (replace-match "\\1# " nil nil)))))
(add-hook 'before-save-hook 'z-remove-fancy-comments)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun z-gdb-mode-hook () (setq gdb-many-windows t))
(add-hook 'gdb-mode-hook 'z-gdb-mode-hook)

(defun z-python-mode-hook () (whitespace-mode 1))
(add-hook 'python-mode-hook 'z-python-mode-hook)

;; --------------------------------------------------
;; Legacy code

;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)
(defun z-cperl-mode-hook ()
  (define-key cperl-mode-map (kbd "C-h f") 'cperl-perldoc)
  (define-key cperl-mode-map "\C-m" 'newline-and-indent)
  (setq cperl-invalid-face nil
        cperl-electric-parens nil
        cperl-electric-keywords nil
        cperl-indent-level 4)
  (define-key cperl-mode-map (kbd "C-x t (")
    (lambda () "toggle electric parens"
      (interactive)
      (toggle-variable 'cperl-electric-parens)))
  (define-key cperl-mode-map (kbd "C-x t )")
    (lambda () "toggle electric keywords"
      (interactive)
      (toggle-variable 'cperl-electric-keywords))))
(add-hook 'cperl-mode-hook 'z-cperl-mode-hook)

(defun z-LaTeX-mode-hook ()
  (define-key LaTeX-mode-map (kbd "C-x `") 'next-error)
  (define-key LaTeX-mode-map (kbd "C-x <f1>") 'TeX-ispell-document)
  (turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        TeX-auto-save t
        TeX-parse-self t))
(add-hook 'LaTeX-mode-hook 'z-LaTeX-mode-hook)
;;'(TeX-PDF-mode t)
;; '(TeX-output-view-style (quote (("^dvi$" "." "evince %dS %d") ("^pdf$" "." "evince %o") ("^html?$" "." "sensible-browser %o"))))
;; '(TeX-view-style (quote (("." "%(o?)evince %dS %d"))))
