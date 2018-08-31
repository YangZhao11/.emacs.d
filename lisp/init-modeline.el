; -*- coding: utf-8; lexical-binding: t -*-

(setq z-lighter-emacs
  '(:propertize (" " (:eval (or current-input-method-title "ε")) " ")
                face (:background "#90E090" :foreground "black")))
(setq z-lighter-mortal
  '(:propertize (" " (:eval (or current-input-method-title "ɪ")) " ")
                face (:background "#80E0D0" :foreground "black")))
(setq z-lighter-view
  '(:propertize " ν "
                face (:background "#E8BB74" :foreground "black")))
(setq z-lighter-special
  '(:propertize (" " (:eval
                      (cond
                       ((eq (local-key-binding "c") 'god-mode-self-insert) "*")
                       ((eq (local-key-binding "x") 'god-mode-self-insert) "×")
                       (t "•"))) " ")
                face (:background "#4D88FF" :foreground "black")))

(defvar z-lighter
  '(:eval (cond (god-local-mode z-god-mode-lighter)
                (mortal-mode z-lighter-mortal)
                (view-mode z-lighter-view)
                ((derived-mode-p 'special-mode 'dired-mode 'Info-mode)
                 z-lighter-special)
                (t z-lighter-emacs)))
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
              (mapcar (lambda (x)
                        (if (and (listp x) (eq (car x) 'current-input-method))
                            "" x))
                      (default-value 'mode-line-mule-info)))

;; mark emacsclient frames using ©
(setq-default mode-line-client '(#1=""
    (:propertize
     (#1# (:eval (if (frame-parameter nil 'client)
                     (if (eq (framep (selected-frame)) 't) "𝕋" "ℂ")
                        #1#)))
      help-echo "emacsclient frame")))

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
