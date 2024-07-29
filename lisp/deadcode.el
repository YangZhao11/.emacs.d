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

;; TODO: hydra for table commands. Ref. "Text Based Tables" in emacs manual.
(use-package table
  :bind ("M-m" . hydra-table/body)
  :config
  (defhydra hydra-table (:color blue :hint nil)
    "
→ TAB    ca_p_ture  _u_n/reco_g_nize   _h_eighten  _w_iden    jus_t_ify   _i_nsert _r_ow/_c_ol   _0_: span
← S-TAB  re_l_ease  _U_n/reco_G_ cell  _s_horten   _n_arrow   se_q_uence  ^^delete _R_ow/_C_ol   _2__3_:split
"
    ("SPC" nil :exit t)
    ("p" table-capture)
    ("l" table-release)
    ("u" table-unrecognize-dwim)
    ("g" table-recognize-dwim)
    ("U" table-unrecognize-cell)
    ("G" table-recognize-cell)
    ("h" table-heighten-cell :exit nil)
    ("s" table-shorten-cell :exit nil)
    ("w" table-widen-cell :exit nil)
    ("n" table-narrow-cell :exit nil)
    ("t" table-justify)
    ("i" table-insert)
    ("q" table-insert-sequence)
    ("r" table-insert-row)
    ("c" table-insert-column)
    ("R" table-delete-row)
    ("C" table-delete-column)
    ("0" table-span-cell)
    ("2" table-split-cell-vertically)
    ("3" table-split-cell-horizontally))

  (defun table-recognize-dwim ()
    (interactive)
    (call-interactively
     (if (use-region-p) 'table-recognize-region
       'table-recognize-table)))
  (defun table-unrecognize-dwim ()
    (interactive)
    (call-interactively
     (if (use-region-p) 'table-unrecognize-region
       'table-unrecognize-table)))
)

(use-package helpful
  :bind (:map help-map
              ("k" . helpful-key)
              ("o" . z-helpful-symbol))
  :bind (:map helpful-mode-map
              ("[" . helpful-previous-heading)
              ("]" . helpful-next-heading)
              ("n" . next-line)
              ("p" . previous-line)
              ("{" . backward-paragraph)
              ("}" . forward-paragraph)
              ("f" . forward-char)
              ("b" . backward-char))
  :config
  (defun helpful-next-heading (&optional arg)
    "Move to next heading"
    (interactive "^p")
    (like-this--next-face 'helpful-heading arg))

  (defun helpful-previous-heading (&optional arg)
    "Move to previous heading"
    (interactive "^p")
    (like-this--next-face 'helpful-heading (- arg)))

    (defun z-helpful-symbol ()
  "Forward to `helpful-symbol'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Describe symbol: " obarray
              :require-match t
              :history 'counsel-describe-symbol-history
              :keymap counsel-describe-map
              :preselect (ivy-thing-at-point)
              :sort t
              :action (lambda (x)
                        (helpful-symbol (intern x)))
              :caller 'counsel-describe-function))))

(defun toggle-one-window ()
    "Change to one window (C-x 1) if applicable, otherwise show other
buffer in other window."
    (interactive)
    (if (window-parent)
        (delete-other-windows)
      (display-buffer (other-buffer) t)))

(use-package lsp-mode :diminish (lsp-mode . " £")
  :config
  (require 'lsp-ui-flycheck))

  (set-register ?5 '(file . "~/Projects/notes/NOTES.org"))

(use-package company :defer 't
  :diminish " ©"
  :commands (company-mode)
  :bind (("M-m" . company-complete))
  :config
  (setq company-idle-delay nil))

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

(use-package grab-region :diminish " ⊛"
  :functions grab-region-move
  :bind ("M-*" . grab-region-mode)
  :config
  (grab-region-remap z-goto-char))


  (defun easy-kill-grab-region ()
    (interactive)
    (easy-kill-mark-region)
    (grab-region-mode))
  (put #'easy-kill-grab-region 'easy-kill-exit t)


(use-package jump-char :ensure
  :bind (("M-." . jump-char-forward)
         ("M-," . jump-char-backward))
  :init
  (setq jump-char-forward-key "M-."
        jump-char-backward-key "M-,")
  :config
  (bind-keys :map jump-char-isearch-map
             ("C-j"      . jump-char-switch-to-ace)
             ("<return>" . jump-char-exit)
             ("RET"      . jump-char-exit))
  (defalias 'ace-jump-char-mode #'avy-goto-char))


(use-package indent-guide
  :diminish indent-guide-mode
  :bind ("C-x t i" . indent-guide-mode)
  :config
  (setq indent-guide-char "│"))

(use-package which-key :ensure :diminish which-key-mode
  :bind ("C-x t /" . which-key-mode)
  :config (setq which-key-idle-delay 2)
  (setq which-key-key-replacement-alist
        (append '(("TAB" . "↹") ("DEL" . "⇤")("RET" . "⏎")("SPC" . "␣"))
                which-key-key-replacement-alist)))

(use-package iy-go-to-char :ensure
  :bind (("M-." . iy-go-up-to-char)
         ("M-," . iy-go-to-char-backward))
  :config
  (setq iy-go-to-char-use-key-forward nil
        iy-go-to-char-use-key-backward nil)
  (defun zy-goto-char-continue (n)
    (interactive "p")
    (iy-go-to-or-up-to-continue
     (* iy-go-to-char-start-dir n) 'exclude))
  (defun zy-goto-char-continue-backward (n)
    (interactive "p")
    (iy-go-to-or-up-to-continue
     (- (* iy-go-to-char-start-dir n)) 'include))
  (bind-keys :map iy-go-to-char-keymap
             ("M-." . zy-goto-char-continue)
             ("M-," . zy-goto-char-continue-backward)))

(use-package expand-region :ensure
  :bind ("C-\\" . er/expand-region)
  :config
  (setq er/try-expand-list
        (append er/try-expand-list '(mark-paragraph mark-page))))

(use-package change-inner :ensure
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package multiple-cursors :ensure
  :init
  (global-unset-key (kbd "M-m"))
  :bind (("M-m ," . mc/mark-more-like-this-extended)
         ("M-m m" . mc/mark-all-dwim)
         ("M-m M-m" . mc/mark-all-like-this-dwim)
         ("M-m /" . mc/edit-lines)
         ("M-m n" . mc/insert-numbers)
         ("M-m ." . mc/mark-pop))
  :config
  (add-to-list 'mc/unsupported-minor-modes 'god-local-mode)
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))


;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)
(defun z-cperl-mode-hook ()
  (define-key cperl-mode-map (kbd "C-h f") 'cperl-perldoc)
  (define-key cperl-mode-map "\C-m" 'newline-and-indent)
  (setq cperl-invalid-face nil
        cperl-electric-parens nil
        cperl-electric-keywords nil
        cperl-indent-level 4))
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

;; (require 'jump-char)
;; (setq jump-char-forward-key "."
;;       jump-char-backward-key ",")
;; (define-key jump-char-isearch-map (kbd "C-j") 'jump-char-switch-to-ace)
;; (defalias 'ace-jump-char-mode 'avy-goto-char)
;; (global-set-key (kbd "M-m") 'jump-char-forward)
;; (global-set-key (kbd "M-M") 'jump-char-backward)

;; replaced with ace-window
(global-set-key (kbd "M-9") (lambda () (interactive)
                              (other-window -1)))
(global-set-key (kbd "M-0") 'other-window)
(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(global-set-key (kbd "M-(") (lambda () (interactive)
                              (transpose-windows -1)))
(global-set-key (kbd "M-)") 'transpose-windows)

;; Set frame font to "Monospace-nn", where nn is the key pressed last
;; with a lookup in font-size-by-digit. That is, if invoked by "C-x
;; C-1" frame font will be set to "Monospace-8".
(setq font-size-by-digit [30 8 9 10 11 12 14 16 19 24])
(setq frame-font-name-format "Liberation Mono-%d")
(defun set-frame-font-size-self-digit (&optional arg)
  (interactive "p")
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0))
         (font-size (if (and (> arg 5) (<= arg 30)) arg
                      (elt font-size-by-digit digit)))
         (font-name (format frame-font-name-format font-size)))
    (set-frame-font font-name t)
    (message "Frame font set to %s" font-name)))

(defun toggle-variable (var)
  "toggles boolean variable"
  (interactive "vToggle variable: ")
  (set var (not (symbol-value var)))
  (message "%s set to %s" var (symbol-value var)))

(setq preferred-debugger-alist
      '((c-mode . gdb)
        (c++-mode . gdb)
        (cperl-mode . perldb)
        (python-mode . pdb)
        (jde-mode . jdb)))
(defun z-maybe-gud ()
  "Run gdb if not already running, otherwise bring it to front"
  (interactive)
  (require 'gud)
  (if (and (boundp 'gud-comint-buffer)  ;find running gdb process
           gud-comint-buffer
           (buffer-name gud-comint-buffer)
           (get-buffer-process gud-comint-buffer))
      (if (fboundp 'gdb-restore-windows)
           (gdb-restore-windows)
        (pop-to-buffer gud-comint-buffer))
    (call-interactively
     (or (cdr (assq major-mode preferred-debugger-alist))
         'gdb))))
(global-set-key (kbd "C-x <f8>") 'z-maybe-gud)

(defun z-maybe-recompile (&optional arg)
  "recompile if possible"
  (interactive "P")
  (if (and (fboundp 'recompile) (not arg))
      (recompile)
    (call-interactively 'compile)))
(global-set-key [f5] 'z-maybe-recompile)

(require 'hydra)

(defhydra hydra-mark (global-map "M-m" :idle 1.0)
  "mark"
  ("s" mark-sexp "sexp")
  ("f" mark-defun "defun")
  ("w" mark-word "word")
  ("e" mark-end-of-sentence "sentence")
  ("h" mark-paragraph "paragraph")
  ("p" mark-page "page")
  ("\\" er/expand-region "expand")
  ("|" er/contract-region "contract")
  ("u" backward-up-list "up list")
  ("d" down-list "down list")
  ("b" backward-sexp "backward list")
  ("f" forward-sexp "forward list")
  ("x" z-exchange-point-and-mark "exchange")
  ("i" z-toggle-activate-mark "activate")
  ("q" nil "cancel")
  )
(global-set-key (kbd "M-m SPC") 'hydra-mark/body)

;; (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
;;                            :color pink
;;                            :hint nil
;;                            :post (deactivate-mark))
;;   "
;;   ^_p_^     _d_elete    s_t_ring  e_x_change
;; _b_   _f_   _k_ill      _y_ank    _/_undo
;;   ^_n_^     ne_w_-copy  _r_eset   _q_uit
;; "
;;   ("b" backward-char)
;;   ("f" forward-char)
;;   ("p" previous-line)
;;   ("n" next-line)
;;   ("x" z-exchange-point-and-mark)
;;   ("w" copy-rectangle-as-kill)
;;   ("d" delete-rectangle)
;;   ("r" (if (region-active-p)
;;            (deactivate-mark)
;;          (rectangle-mark-mode 1)))
;;   ("y" yank-rectangle)
;;   ("/" undo)
;;   ("t" string-rectangle)
;;   ("k" kill-rectangle)
;;   ("q" nil))
;; (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

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
    ;;(company-mode 1)
    ;; somehow eshell-mode-map is buffer-local
    (bind-keys :map eshell-mode-map
             ("M-r" . counsel-esh-history)))
  (add-hook 'eshell-mode-hook #'z-eshell-mode-hook))

(use-package scala2-mode
  :config
  (defun z-scala-mode-hook ()
    (setq prettify-symbols-alist
          (append '(("=>" . ?⇒)
                    ("->" . ?→))
                  prettify-symbols-alist))
    (prettify-symbols-mode))
  (add-hook 'scala-mode-hook #'z-scala-mode-hook))

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

(use-package imenu-anywhere
  :bind ("M-g M-i" . ivy-imenu-anywhere))


(use-package imenu-list
  :bind ("M-s l" . z-imenu-list)
  :config
  (setq imenu-list-position 'left
        imenu-list-size 30)
  (defun z-imenu-list ()
    "toggle imenu-list, but switch off ibuffer-sidebar and dired-sidebar first."
    (interactive)
    (dired-sidebar-hide-sidebar)
    (ibuffer-sidebar-hide-sidebar)
    (imenu-list-smart-toggle)))

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
      (propertize "✔" 'face 'compilation-mode-line-exit)
    (list ""
     (propertize (z-status-count "✖" nerror)
                 'face 'compilation-error)
     (if (and (< 0 nwarning) (< 0 nerror)) "/" "")
     (propertize (z-status-count "▴" nwarning)
                 'face 'compilation-warning)
     (if (and (or (< 0 nwarning) (< 0 nerror)) (< 0 ninfo)) "/" "")
     (propertize (z-status-count "•" ninfo)
                 'face 'compilation-info))))

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
                     (propertize "✔" 'face 'compilation-mode-line-exit)))
                  (`running     (propertize "✔" 'face 'compilation-mode-line-run))
                  (`not-checked (propertize "✔" 'face 'compilation-error))
                  (`no-checker  "¿")
                  (`errored     (propertize "‼" 'face 'compilation-mode-line-fail))
                  (`interrupted (propertize "⁉" 'face 'compilation-mode-line-fail))
                  (`suspicious  "‽"))))
      (list " " pick)))
  (setq flycheck-mode-line '(:eval (z-flycheck-mode-line-text)))
  (bind-keys :map flycheck-mode-map
             ("M-g k"   . consult-flycheck)
             ("M-g M-k" . flycheck-list-errors)
             ("M-g f"   . flycheck-next-error)
             ("M-g b"   . flycheck-previous-error)))
