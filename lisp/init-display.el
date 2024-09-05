; -*- coding: utf-8; lexical-binding: t -*-

(defface god-lighter
  '((t :inherit mode-line :foreground "black"))
  "Face for god-lighter")

(defmacro z-defface-with-darken (face col)
  "Define two faces, FACE and FACE-dark where bg is col and a dark version
of it."
  (let ((face-dark (intern (concat (symbol-name face) "-dark"))))
    `(progn
       (defface ,face
       '((t :inherit god-lighter :background ,col))
       ,(concat "Face for " (symbol-name face)))
       (defface ,face-dark
       '((t :inherit god-lighter :background ,(doom-darken col .3)))
     ,(concat "Face for " (symbol-name face-dark)))))
  )

(z-defface-with-darken god-lighter-emacs "#90E090")

(setq z-lighter-emacs
      '(:eval
        (propertize (concat " " (or current-input-method-title "ɛ") " ")
                    'face (if (mode-line-window-selected-p)
                              'god-lighter-emacs 'god-lighter-emacs-dark))))

(z-defface-with-darken god-lighter-god "#4DB0FF")

(setq z-lighter-god
      '(:eval
        (propertize (let ((m (cdr (assoc nil god-mod-alist))))
                              (cond ((string= m "C-") " ⌘ ")
                                    ((string= m "C-M-") "⌥⌘ ")
                                    ('t " ⌥ ")))
                    'face (if (mode-line-window-selected-p)
                             'god-lighter-god 'god-lighter-god-dark))))

(z-defface-with-darken god-lighter-mortal "#88E0C0")
(setq z-lighter-mortal
      '(:eval (propertize
               (concat " " (or current-input-method-title "I") " ")
               'face (if (mode-line-window-selected-p)
                         'god-lighter-mortal 'god-lighter-mortal-dark))))

(z-defface-with-darken god-lighter-view "#D8E874")
(setq z-lighter-view
  '(:eval (propertize "⊙⊙ "
                      'face (if (mode-line-window-selected-p)
                                'god-lighter-view 'god-lighter-view-dark))))

(z-defface-with-darken god-lighter-special "#6B77FF")

(setq z-lighter-special
      '(:eval (propertize
               (concat
                " "
                 (cond
                   ((eq (local-key-binding "x") 'god-mode-self-insert)
                    (if (eq (local-key-binding "c") 'god-mode-self-insert)
                        "*" "×"))
                   (:else "•"))
                 " ")
               'face (if (mode-line-window-selected-p)
                         'god-lighter-special 'god-lighter-special-dark))))

(defvar z-lighter
  '(:eval (cond (god-local-mode z-lighter-god)
                (mortal-mode z-lighter-mortal)
                (view-mode z-lighter-view)
                ((derived-mode-p 'special-mode 'dired-mode
                                 'Info-mode 'ess-help-mode)
                 z-lighter-special)
                (:else z-lighter-emacs)))
  "Leftmost lighter in mode line")

(setq mode-line-frame-name
      ;; Show frame name in text frame, in the bottom right corner,
      ;; i.e. next window is minibuffer.
      '(:eval
        (when (and (not (display-graphic-p))
                   (window-minibuffer-p (next-window nil 't))) "%F")))

(setq-default mode-line-format
'("%e"
 (:eval z-lighter)
 mode-line-front-space mode-line-mule-info
 ;; mode-line-client
 mode-line-modified
 mode-line-remote " " ;mode-line-frame-identification
 mode-line-buffer-identification "   " mode-line-position
 (vc-mode vc-mode)
 "  " mode-line-modes mode-line-misc-info
 mode-line-format-right-align
 mode-line-frame-name))

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
          (file-remote-p default-directory))
     (propertize
      "@"
      'mouse-face 'mode-line-highlight
      'help-echo (purecopy (lambda (window _object _point)
                             (concat "Current directory is remote: "
                                     default-directory)))))

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
