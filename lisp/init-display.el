; -*- coding: utf-8; lexical-binding: t -*-

(defface god-lighter
  '((t :inherit mode-line :foreground "black" :group god))
  "Face for god-lighter")


(defmacro z-defface-with-darken (face col)
  "Define two faces, FACE and FACE-dark where bg is col and a dark version
of it."
  (let ((face-dark (intern (concat (symbol-name face) "-dark"))))
    `(progn
       (defface ,face
       '((t :inherit god-lighter :background ,col :group god))
       ,(concat "Face for " (symbol-name face)))
       (defface ,face-dark
         ;; blend with (face-background 'mode-line-inactive)
       '((t :inherit god-lighter :background ,(doom-blend col "#353535" .5) :group god))
     ,(concat "Face for " (symbol-name face-dark)))))
  )

(z-defface-with-darken god-lighter-emacs "#90E090")
(z-defface-with-darken god-lighter-mortal "#88E0C0")
(setq z-lighter-emacs
      '(:eval
        (propertize
         (if mortal-mode " I " " ɛ ")
         'face (let ((s (mode-line-window-selected-p))
                     (m mortal-mode))
                 (if m
                     (if s 'god-lighter-mortal 'god-lighter-mortal-dark)
                   (if s 'god-lighter-emacs 'god-lighter-emacs-dark)))
         'help-echo (if mortal-mode
                        "Insert mode, \\[mortal-mode-exit] to exit"
                     "Emacs mode"))))
(put 'z-lighter-emacs 'risky-local-variable t)
(setq z-lighter-input-method
      '(:eval
        (propertize
         (concat
              (if (<= (length current-input-method-title) 1) " " "")
              ;; ref mode-line-mule-info
              current-input-method-title
              (if (<= (length current-input-method-title) 2) " " ""))
         'face (let ((s (mode-line-window-selected-p))
                     (m mortal-mode))
                 (if m
                     (if s 'god-lighter-mortal 'god-lighter-mortal-dark)
                   (if s 'god-lighter-emacs 'god-lighter-emacs-dark)))
         'help-echo (concat
		     (purecopy "Current input method: ")
		     current-input-method
		     (purecopy "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
                     (if mortal-mode
                         "\nInsert mode, \\[mortal-mode-exit] to exit"))
         'local-map mode-line-input-method-map)))
(put 'z-lighter-input-method 'risky-local-variable t)

(z-defface-with-darken god-lighter-god "#4DB0FF")
(setq z-lighter-god
      '(:eval
        (propertize
         (let ((m (cdr (assoc nil god-mod-alist))))
           (cond ((string= m "C-") " ⌘ ")
                 ((string= m "C-M-") "⌥⌘ ")
                 ((string= m "M-") " ⌥ ")
                 ('t " ? ")))
         'face (if (mode-line-window-selected-p)
                   'god-lighter-god 'god-lighter-god-dark)
         'help-echo "\\[god-mode-toggle-sticky-meta]: Sticky M-\n\
\\[god-mode-toggle-sticky-cm]: Sticky C-M-")))
(put 'z-lighter-god 'risky-local-variable t)

(defun z-lighter-arrow-char ()
  "Arrow char for the lighter"
  (let ((ud (and
             (memq (key-binding "j")
                   '(scroll-up-command Info-scroll-up View-scroll-page-forward))
             (memq (key-binding "k")
                   '(scroll-down-command Info-scroll-down View-scroll-page-backward))))
        (lr (and
             (eq (key-binding "a") 'move-beginning-of-line)
             (eq (key-binding "e") 'move-end-of-line))))
    (cond
     ((and ud lr) "+")
     (ud "↕")
     (lr "↔")
     (:else " "))))

(z-defface-with-darken god-lighter-view "#D8E874")
(setq z-lighter-view
      '(:eval (propertize
               (concat
                (z-lighter-arrow-char)
                "ν ")
               'face (if (mode-line-window-selected-p)
                         'god-lighter-view 'god-lighter-view-dark)
               'help-echo "View mode (\\[view-mode])")))
(put 'z-lighter-view 'risky-local-variable t)

(z-defface-with-darken god-lighter-special "#6B77FF")
(setq z-lighter-special
      '(:eval
        (propertize
         (concat
          (z-lighter-arrow-char)
          (let ((x (eq (key-binding "x") 'god-mode-self-insert))
                (c (eq (key-binding "c") 'god-mode-self-insert)))
            (cond
             ((and x c) "*")
             (x "×")
             (c "c")
             (:else "•")))
          " ")
         'face (if (mode-line-window-selected-p)
                   'god-lighter-special 'god-lighter-special-dark))))
(put 'z-lighter-special 'risky-local-variable t)

(defvar z-lighter
  '(:eval (cond (god-local-mode z-lighter-god)
                (view-mode z-lighter-view)
                ((derived-mode-p 'special-mode 'dired-mode
                                 'Info-mode 'ess-help-mode
                                 'emacs-news-view-mode)
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
 mode-line-front-space mode-line-mule-info
 ;; mode-line-client
 mode-line-modified
 mode-line-remote
 mode-line-window-dedicated
" " ;mode-line-frame-identification
 mode-line-buffer-identification "   " mode-line-position
 "  " mode-line-modes mode-line-misc-info
 mode-line-format-right-align
 (vc-mode vc-mode)
 (project-mode-line project-mode-line-format)
 " "
; mode-line-frame-name
))
(setq project-mode-line 't)
(setq project-mode-line-face 'font-lock--face)

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

(setq-default mode-line-modified
  '(:eval (z-buffer-status 't)))

(setq-default mode-line-remote
  '(:eval (cond
    ((bound-and-true-p edit-server-edit-mode)
     (propertize
      "&"
      'mouse-face 'mode-line-highlight
      'help-echo "Editing browser content"))

    ((and (stringp default-directory)
          (file-remote-p default-directory)
          (string-prefix-p "/ssh:cloud:" default-directory))
     (propertize
      "ℂ"
      'face 'font-lock-type-face
      'mouse-face 'mode-line-highlight
      'help-echo (purecopy (lambda (window _object _point)
                             (concat "Current directory is on cloud: "
                                     default-directory)))))

    ((and (stringp default-directory)
          (file-remote-p default-directory))
     (propertize
      "@"
      'mouse-face 'mode-line-highlight
      'help-echo (purecopy (lambda (window _object _point)
                             (concat "Current directory is remote: "
                                     default-directory)))))

    ((bound-and-true-p crdt--session)
     (propertize
      "‰"
      'mouse-face 'mode-line-highlight
      'help-echo "CRDT shared buffer"))

    ((bound-and-true-p server-buffer-clients)
     (propertize
      "#"
      'mouse-face 'mode-line-highlight
      'help-echo "Client waiting for edit")))))


(setq overlay-arrow-string "‣")

;; `Narrow' in mode line changed to §
(setq mode-line-modes
      (mapcar (lambda (x)
                (if (and (stringp x) (string= x "%n"))
                    `(:propertize (:eval (if (buffer-narrowed-p) " §"))
                        help-echo "mouse-2: Remove narrowing from buffer"
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
