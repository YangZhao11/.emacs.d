;;; keymap-hint --- Show hint for keymap -*- lexical-binding: t -*-

(require 'lv)

(defun propertize-regexp (string regexp &rest properties)
  "Propertize STRING capatured by REGEXP.

If REGEXP has a capture group, then the match will be replaced by the
capture group. PROPERTIES are passed to `propertize' directly."
  (let ((start 0))
    (while (and (< start (length string))
                (string-match regexp string start))
      (if-let* ((matched (match-string 1 string)))
          (progn
            (setq string
                  (replace-match
                   (apply #'propertize matched properties)
                   t nil string))
            (setq start (+ (match-beginning 0) (length matched))))
        :else                    ; nothing to replace, just propertize
        (add-text-properties
         (match-beginning 0) (match-end 0) properties string)
        (setq start (match-end 0))))
    string))


(defun keymap-hint-hide ()
  "Hide current hint."
  (interactive)
  (lv-delete-window))

(defvar-keymap keymap-hint-transient-map
  "SPC" #'keymap-hint-hide)

(defun keymap-hint--keep-hint ()
  "Decide if we keep the transient map."
  (cond
   ((eq this-command 'keymap-hint-hide) nil)
   ((memq (get this-command 'command-semantic)
          '(switch-buffer switch-window))
    (keymap-hint-hide)
    nil)
   (:else t)))

;;;###autoload
(defun keymap-hint-show (hint)
  "Show mode hint attached to major mode symbol if present."
  (interactive)
  (if (symbolp hint)
      (setq hint (get hint 'hint)))
  (unless (stringp hint)
    (setq hint (eval hint 't)))
  (when hint
      (lv-message hint)
      (set-transient-map
       keymap-hint-transient-map 'keymap-hint--keep-hint)))

;;;###autoload
(defmacro keymap-hint-set (keymap key hint)
  "Set HINT for keymap."
  (unless (or (keymapp (eval keymap))
              (and (symbolp keymap) (keymapp (symbol-value keymap))))
     (error "Need a keymap to show hint."))
  (setq hint (string-trim hint))
  (setq hint (replace-regexp-in-string "◦" "" hint))
  (setq hint (propertize-regexp hint "_\\([^_]+\\)_" 'face 'font-lock-function-name-face))

  (if (symbolp keymap)
      `(progn
        (put (quote ,keymap) 'hint ,hint)
        (keymap-set ,keymap ,key
                     (lambda () ,(concat "Show hint for `" (symbol-name keymap) "'.")
                       (interactive)
                       (keymap-hint-show (quote ,keymap)))))
    ;; else
    `(keymap-set ,keymap ,key
                 (lambda () "Show hint for keymap"
                   (interactive)
                   (keymap-hint-show ,hint)))))


  (unless (keymapp bookmark-bmenu-mode-map)
    (error "Need a keymap to show hint."))



(provide 'keymap-hint)
