; -*- coding: utf-8; lexical-binding: t -*-
(eval-when-compile
  ;; for `doom-blend'
  (require 'doom-themes))

(defface god-lighter
  '((t :inherit mode-line :foreground "black"))
  "Face for god-lighter"
  :group 'god)


;; e0a 
;; e0b 

(defmacro z-defface-with-darken (face col)
  "Define two faces, FACE and FACE-dark where bg is col and a dark version
of it."
  (let ((face-dark (intern (concat (symbol-name face) "-dark")))
        (face-separator (intern (concat (symbol-name face) "-separator")))
        (face-dark-separator (intern (concat (symbol-name face) "-dark-separator")))
        (dark-col (doom-blend col "#353535" .5)))
    `(progn
       (defface ,face
       '((t :inherit god-lighter :background ,col))
       ,(concat "Face for " (symbol-name face))
       :group 'god )
       (defface ,face-separator
         '((t :inherit mode-line-active :foreground ,col))
         ,(concat "Separator face for " (symbol-name face)))
       (defface ,face-dark
         ;; blend with (face-background 'mode-line-inactive)
       '((t :inherit god-lighter :background ,dark-col))
       ,(concat "Face for " (symbol-name face-dark))
        :group 'god)
       (defface ,face-dark-separator
         '((t :inherit mode-line-inactive :foreground ,dark-col))
         ,(concat "Separator face for " (symbol-name face-dark))))))

(z-defface-with-darken god-lighter-emacs "#90E090")
(z-defface-with-darken god-lighter-mortal "#88E0C0")

(defun z-lighter-emacs ()
  (let* ((s (mode-line-window-selected-p))
         (m mortal-mode)
         (tag-face (if m (if s 'god-lighter-mortal 'god-lighter-mortal-dark)
                     (if s 'god-lighter-emacs 'god-lighter-emacs-dark)))
         (sep-face (if m
                       (if s 'god-lighter-mortal-separator
                         'god-lighter-mortal-dark-separator)
                     (if s 'god-lighter-emacs-separator
                       'god-lighter-emacs-dark-separator))))
    (propertize
     (concat
      (propertize
       (if mortal-mode " ɪ" " ɛ") 'face tag-face)
      (propertize "" 'face sep-face))
     'help-echo (if m
                    "Insert mode, \\[mortal-mode-exit] to exit"
                  "Emacs mode"))))

(setq z-lighter-emacs '(:eval (z-lighter-emacs)))
(put 'z-lighter-emacs 'risky-local-variable t)

(defun z-lighter-input-method ()
  (let* ((left-padding (if (<= (length current-input-method-title) 1) " " ""))
        (s (mode-line-window-selected-p))
        (m mortal-mode)
        (tag-face (if m
                      (if s 'god-lighter-mortal 'god-lighter-mortal-dark)
                    (if s 'god-lighter-emacs 'god-lighter-emacs-dark)))
        (sep-face (if m
                      (if s 'god-lighter-mortal-separator
                        'god-lighter-mortal-dark-separator)
                    (if s 'god-lighter-emacs-separator
                      'god-lighter-emacs-dark-separator))))
    (propertize
     (concat
        ;; ref mode-line-mule-info
      (propertize (concat left-padding current-input-method-title)
       'face tag-face)
      (propertize "" 'face sep-face))
      'help-echo (concat "Current input method: "
	          current-input-method
	          "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method"
                  (if mortal-mode
                      "\nInsert mode, \\[mortal-mode-exit] to exit"))
      'local-map mode-line-input-method-map)))

(setq z-lighter-input-method '(:eval (z-lighter-input-method)))
(put 'z-lighter-input-method 'risky-local-variable t)

(z-defface-with-darken god-lighter-god "#4DB0FF")

(defun z-lighter-god-mod-char ()
  (let ((m (cdr (assoc nil god-mod-alist))))
           (cond ((string= m "C-") " ⌘")
                 ((string= m "C-M-") "⌥⌘")
                 ((string= m "M-") " ⌥")
                 ('t " ?"))))
(defun z-lighter-god ()
  (let* ((s (mode-line-window-selected-p))
         (tag-face (if s 'god-lighter-god
                     'god-lighter-god-dark))
         (sep-face (if s 'god-lighter-god-separator
                     'god-lighter-god-dark-separator)))
      (propertize
         (concat
          (propertize (z-lighter-god-mod-char) 'face tag-face)
          (propertize "" 'face sep-face))
         'help-echo "\\[god-mode-toggle-sticky-meta]: Sticky M- prefix\n\
\\[god-mode-toggle-sticky-cm]: Sticky C-M- prefix")))

(setq z-lighter-god '(:eval (z-lighter-god)))
(put 'z-lighter-god 'risky-local-variable t)

(defun z-lighter-arrow-char ()
  "Arrow char for the lighter"
  (let ((jk (and
             (eq (get (key-binding "j") 'command-semantic) 'scroll-up-command)
             (eq (get (key-binding "k") 'command-semantic) 'scroll-down-command)))
        (np (and
             (eq (get (key-binding "n") 'command-semantic) 'next-line)
             (eq (get (key-binding "p") 'command-semantic) 'previous-line)))
        (ae (and
             (eq (key-binding "a") 'move-beginning-of-line)
             (eq (key-binding "e") 'move-end-of-line))))
    (cond
     ((and jk np ae) "⧺")
     ((and jk ae) "+")
     ((and jk np) "⇕")
     (jk "↕")
     ((and ae np) "÷")
     (ae "-")
     (np "❘")
     (:else " "))))

(z-defface-with-darken god-lighter-view "#D8E874")
(defun z-lighter-view ()
  (let* ((s (mode-line-window-selected-p))
         (tag-face (if s 'god-lighter-view 'god-lighter-view-dark))
         (sep-face (if s 'god-lighter-view-separator
                     'god-lighter-view-dark-separator)))
    (propertize
     (concat
      (propertize
       (concat (z-lighter-arrow-char) "ν")
       'face tag-face)
      (propertize "" 'face sep-face))
     'help-echo "View mode (\\[view-mode])")))

(setq z-lighter-view '(:eval (z-lighter-view)))
(put 'z-lighter-view 'risky-local-variable t)

(defun z-lighter-xc-char ()
  (let ((x (eq (key-binding "x") 'god-mode-self-insert))
        (c (eq (key-binding "c") 'god-mode-self-insert)))
    (cond
     ((and x c) "*")
     (x "×")
     (c "c")
     (:else "•"))))

(z-defface-with-darken god-lighter-special "#6B77FF")
(defun z-lighter-special ()
  (let* ((s (mode-line-window-selected-p))
         (tag-face (if s 'god-lighter-special
                     'god-lighter-special-dark))
         (sep-face (if s 'god-lighter-special-separator
                     'god-lighter-special-dark-separator)))
    (concat
     (propertize
         (concat
          (z-lighter-arrow-char)
          (z-lighter-xc-char))
         'face tag-face)
     (propertize "" 'face sep-face))))
(setq z-lighter-special '(:eval (z-lighter-special)))
(put 'z-lighter-special 'risky-local-variable t)

(defvar z-lighter
  '(:eval (cond (god-local-mode z-lighter-god)
                (view-mode z-lighter-view)
                ((or (and (eq (get major-mode 'mode-class) 'special)
                          (not (derived-mode-p 'comint-mode)))
                     (derived-mode-p 'special-mode
                                     'emacs-news-view-mode))
                 z-lighter-special)
                (current-input-method z-lighter-input-method)
                (:else z-lighter-emacs)))
  "Leftmost lighter in mode line")
(put 'z-lighter 'risky-local-variable t)

(setq mode-line-frame-name
      ;; Show frame name in text frame, in the bottom right corner,
      ;; i.e. next window is minibuffer.
      '(:eval
        (when (and (not (display-graphic-p))
                   (window-minibuffer-p (next-window nil 't)))
          "%F")))
(put 'mode-line-frame-name 'risky-local-variable 't)

(setq-default mode-line-format
'("%e"
 (:eval z-lighter)
 " " mode-line-mule-info
 ;; mode-line-client
 mode-line-modified
 mode-line-remote
; mode-line-window-dedicated
" " ;mode-line-frame-identification
 mode-line-buffer-identification "   " mode-line-position
 "  " mode-line-modes mode-line-misc-info
 mode-line-format-right-align
 (vc-mode vc-mode)
 ;;(project-mode-line project-mode-line-format)
 (:eval (mode-line-window-side))
 (:eval (mode-line-window-state))
 " "
; mode-line-frame-name
))
(setq project-mode-line 't)
;(setq project-mode-line-face 'font-lock-comment-face)

(defvar window-side-lighter
  '((top . "⬒")
    (bottom . "⬓")
    (left . "◧")
    (right . "◨")))

(defvar-keymap mode-line-window-side-keymap
  :doc "Keymap for what is displayed by `mode-line-window-side'."
  "<mode-line> <mouse-1>" #'window-toggle-side-windows)

(defun mode-line-window-side ()
  "Mode line string for window sides"
  (when-let* ((s (window-parameter (selected-window) 'window-side)))
    (propertize (alist-get s window-side-lighter)
                'help-echo "Side window\nmouse-1: Toggle all"
                'local-map mode-line-window-side-keymap
                'mouse-face 'mode-line-highlight)))

(defun mode-line-window-state ()
  "Mode line string for window state, including dedicated, project."
  (or
   (if (window-dedicated-p)
    (concat "[" (mode-line-window-control) "]"))
   (if (window-parameter (selected-window) 'window-role)
       (mode-line-window-role))
   (if (and project-mode-line
            buffer-file-name
            (not (file-remote-p buffer-file-name)))
       (project-mode-line-format))))


;; remove input method from mode-line-mule-info, this is already
;; handled by z-lighter.
(setq-default mode-line-mule-info
  `(""
    ,(propertize
      "%z"
      'help-echo 'mode-line-mule-info-help-echo
      'mouse-face 'mode-line-highlight
      'local-map mode-line-coding-system-map)
    (:eval (mode-line-eol-desc))))

(setq-default mode-line-modified '(:eval (z-buffer-status 't)))

(defun mode-line-remote ()
  (let ((char-help
         (cond
          ((bound-and-true-p edit-server-edit-mode)
           '("&" "Editing browser content"))
          ((and (stringp default-directory)
                (file-remote-p default-directory)
                (string-prefix-p "/ssh:cloud:" default-directory))
           `("ℂ" ,(concat "Current directory is on cloud: "
                          default-directory)))
          ((and (stringp default-directory)
                (file-remote-p default-directory))
           `("@" ,(concat "Current directory is remote: "
                          default-directory)))
          ((bound-and-true-p crdt--session)
           '("‰" "CRDT shared buffer"))
          ((bound-and-true-p server-buffer-clients)
           '("#" "Client waiting for edit")))))
    (when char-help
      (propertize
       (car char-help)
       'mouse-face 'mode-line-highlight
       'help-echo (cadr char-help)))
))
(setq-default mode-line-remote '(:eval (mode-line-remote)))


(setq overlay-arrow-string "‣")

;; `Narrow' in mode line changed to §
(setq mode-line-modes
      (mapcar (lambda (x)
                (if (and (stringp x) (string= x "%n"))
                    `(:propertize (:eval (if (buffer-narrowed-p) " §"))
                                  help-echo "mouse-2: Remove narrowing from buffer\n\\[narrow-dwim] to toggle narrowing"
                                  mouse-face mode-line-highlight
                                  local-map ,(make-mode-line-mouse-map
                                              'mouse-2 #'mode-line-widen)
                                  face warning)
                  x))
              mode-line-modes))

;; line wrap symbol under terminal
(or standard-display-table (setq standard-display-table (make-display-table)))
(set-display-table-slot standard-display-table 'truncation ?›)
(set-display-table-slot standard-display-table 'wrap ?↵)
(set-display-table-slot standard-display-table 'selective-display [?…])
(set-display-table-slot standard-display-table 'vertical-border ?│)
;; tty box unicode from 31
(when (fboundp #'standard-display-unicode-special-glyphs)
  (standard-display-unicode-special-glyphs))
