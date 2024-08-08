; -*- coding: utf-8; lexical-binding: t -*-

(defface god-lighter
  '((t :inherit mode-line :foreground "black"))
  "Face for god-lighter")

(defface god-lighter-emacs
  '((t :inherit god-lighter :background "#90E090"))
  "Face for god-lighter emacs mode")

(defface god-lighter-emacs-inactive
  '((t :inherit god-lighter :background "#50A050"))
  "Face for god-lighter emacs mode")

(setq z-lighter-emacs
      '(:eval
        (propertize (concat " " (or current-input-method-title "ɛ") " ")
                    'face (if (mode-line-window-selected-p)
                              'god-lighter-emacs
                            'god-lighter-emacs-inactive))))

(defface god-lighter-god
  '((t :inherit god-lighter :background "#4DB0FF"))
  "Face for god-lighter emacs mode")

(defface god-lighter-god-inactive
  '((t :inherit god-lighter :background "#397CC0"))
  "Face for god-lighter emacs mode")

(setq z-lighter-god
      '(:eval
        (propertize (let ((m (cdr (assoc nil god-mod-alist))))
                              (cond ((string= m "C-") " ⌘ ")
                                    ((string= m "C-M-") "⌥⌘ ")
                                    ('t " ⌥ ")))
                    'face (if (mode-line-window-selected-p)
                             'god-lighter-god 'god-lighter-god-inactive))))

(defface god-lighter-mortal
  '((t :inherit god-lighter :background "#88E0C0"))
  "Face for god-lighter emacs mode")

(defface god-lighter-mortal-inactive
  '((t :inherit god-lighter :background "#66A890"))
  "Face for god-lighter emacs mode")

(setq z-lighter-mortal
      '(:eval (propertize
               (concat " " (or current-input-method-title "I") " ")
               'face (if (mode-line-window-selected-p)
                         'god-lighter-mortal 'god-lighter-mortal-inactive))))

(defface god-lighter-view
  '((t :inherit god-lighter :background "#D8E874"))
  "Face for god-lighter emacs mode")

(defface god-lighter-view-inactive
  '((t :inherit god-lighter :background "#6C743A"))
  "Face for god-lighter emacs mode")

(setq z-lighter-view
  '(:eval (propertize "ʘʘ "
                      'face (if (mode-line-window-selected-p)
                                'god-lighter-view 'god-lighter-view-inactive))))

(defface god-lighter-special
  '((t :inherit god-lighter :background "#4D88FF"))
  "Face for god-lighter emacs mode")

(defface god-lighter-special-inactive
  '((t :inherit god-lighter :background "#3966C0"))
  "Face for god-lighter emacs mode")

(setq z-lighter-special
      '(:eval (propertize
               (concat " "
                 (cond
                   ((eq (local-key-binding "x") 'god-mode-self-insert)
                    (if (eq (local-key-binding "c") 'god-mode-self-insert)
                        "*" "×"))
                   (:else "•"))
                 " ")
               'face (if (mode-line-window-selected-p)
                         'god-lighter-special 'god-lighter-special-inactive))))

(defvar z-lighter
  '(:eval (cond (god-local-mode z-lighter-god)
                (mortal-mode z-lighter-mortal)
                (view-mode z-lighter-view)
                ((derived-mode-p 'special-mode 'dired-mode
                                 'Info-mode 'ess-help-mode)
                 z-lighter-special)
                (:else z-lighter-emacs)))
  "Leftmost lighter in mode line")

(setq-default mode-line-format
'("%e"
 (:eval z-lighter)
 mode-line-front-space mode-line-mule-info
 ;; mode-line-client
 mode-line-modified
 mode-line-remote " " ;mode-line-frame-identification
 mode-line-buffer-identification "   " mode-line-position
 (vc-mode vc-mode)
 "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

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
  '(:eval (cond

    ((derived-mode-p 'comint-mode 'term-mode)
     (propertize
      "∞"
      'help-echo "Interactive shell"))

    (buffer-read-only
     (propertize
      "∅"
      'help-echo 'mode-line-read-only-help-echo
      'local-map (purecopy (make-mode-line-mouse-map
                            'mouse-1
                            #'mode-line-toggle-read-only))
      'mouse-face 'mode-line-highlight))

    ((buffer-modified-p)
     (propertize
      "♦"
      'help-echo 'mode-line-modified-help-echo
      'local-map (purecopy (make-mode-line-mouse-map
                            'mouse-1 #'mode-line-toggle-modified))
      'mouse-face 'mode-line-highlight))
    (:else
     (propertize
      "♢"
      'help-echo "Buffer is not modified")))))

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
