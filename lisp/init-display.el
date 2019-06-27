; -*- coding: utf-8; lexical-binding: t -*-

(setq z-lighter-emacs
  '(:propertize (" " (:eval (or current-input-method-title "ε")) " ")
                face (:background "#90E090" :foreground "black")))
(setq z-lighter-god
  '(:propertize " ⌘ "
                face (:background "#4DB0FF" :foreground "black")))
(setq z-lighter-mortal
  '(:propertize (" " (:eval (or current-input-method-title "ɪ")) " ")
                face (:background "#88E0C0" :foreground "black")))
(setq z-lighter-view
  '(:propertize " ν "
                face (:background "#E8BB74" :foreground "black")))
(setq z-lighter-special
  '(:propertize (" " (:eval
                      (cond
                       ((eq (local-key-binding "c") 'god-mode-self-insert) "*")
                       ((eq (local-key-binding "x") 'god-mode-self-insert) "×")
                       (:else "•"))) " ")
                face (:background "#4D88FF" :foreground "black")))

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

(setq-default mode-line-modified
              '(:eval (cond (buffer-read-only
                             (propertize
                              "♢"
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
                            (:else " "))))

(setq-default mode-line-remote
              '(:eval (cond ((bound-and-true-p edit-server-edit-mode)
                             (propertize
                              "☢"
                              'mouse-face 'mode-line-highlight
                              'help-echo "Editing browser content"))
                            ((and (stringp default-directory)
                                  (file-remote-p default-directory))
                             (propertize
                              "☯"
                              'mouse-face 'mode-line-highlight
                              'help-echo (purecopy (lambda (window _object _point)
                                                     (concat "Current directory is remote: "
                                                             default-directory))))))))


;; remove input method from mode-line-mule-info, this is already
;; handled by z-lighter.
(setq-default mode-line-mule-info
              (mapcar (lambda (x)
                        (if (and (listp x) (eq (car x) 'current-input-method))
                            "" x))
                      (default-value 'mode-line-mule-info)))

(setq overlay-arrow-string "►")

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
(set-display-table-slot standard-display-table 'truncation ?↔)
(set-display-table-slot standard-display-table 'wrap ?↵)
(set-display-table-slot standard-display-table 'selective-display [?…])
(set-display-table-slot standard-display-table 'vertical-border ?│)
