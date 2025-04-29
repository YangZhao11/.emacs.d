;;; keymap-hint --- Show hint for keymap -*- lexical-binding: t -*-

(require 'lv)

(defvar keymap-hint--current nil
  "Current hint that is showing")

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
  (setq keymap-hint--current nil)
  (lv-delete-window))

(defvar-keymap keymap-hint-transient-map
  "SPC" #'keymap-hint-hide)


(defun keymap-hint--keep-hint ()
  "Decide if we keep the transient map."
  (cond
   ;; this-command might be a lambda.
   ;; TODO: turn off transient map when lambda of keymap-hint-show is called.
   ((not (symbolp this-command)) 't)
   ((eq this-command 'keymap-hint-hide) nil)
   ((eq (get this-command 'command-semantic) 'keymap-hint-show) nil)
   ((memq (get this-command 'command-semantic)
          '(switch-buffer switch-window))
    (keymap-hint-hide)
    nil)
   (:else 't)))

;;;###autoload
(defun keymap-hint-show (hint &optional load-map)
  "Show mode hint attached to major mode symbol if present.

LOAD-MAP is active as transient map afterwards."
  (interactive)
  (if (eq hint keymap-hint--current)
      (keymap-hint-hide)
    :else
    (setq keymap-hint--current hint)
  (if (symbolp hint)
      (setq hint (get hint 'hint)))
  (unless (stringp hint)
    (setq hint (eval hint 't)))
  (when hint
      (lv-message hint)
      (set-transient-map
       (or load-map
           keymap-hint-transient-map)
       'keymap-hint--keep-hint
))))

;;;###autoload
(defmacro keymap-hint-set (keymap key hint &optional load)
  "Set HINT for KEYMAP, which is a symbol of keymap name.

Bind KEY in KEYMAP to show hint. If LOAD is non-nil, the keymap is
loaded after showing hint."
  (unless (and (symbolp keymap) (keymapp (symbol-value keymap)))
    (error "KEYMAP should be a symbol to a keymap."))
  (setq hint (string-trim hint))
  (setq hint (replace-regexp-in-string "◦" "" hint))
  (setq hint (propertize-regexp hint "_\\([^_]+\\)_" 'face 'font-lock-function-name-face))

  (let ((show-hint-symbol
         (intern (concat (symbol-name keymap) "-hint"))))
    `(progn
       (put (quote ,keymap) 'hint ,hint)
       (defun ,show-hint-symbol ()
         ,(concat "Show hint for `" (symbol-name keymap) "'.")
         (interactive)
         (keymap-hint-show
          (quote ,keymap)
          ,(if load
               keymap)))
       (put (quote ,show-hint-symbol) 'command-semantic 'keymap-hint-show)
       (keymap-set ,keymap ,key (quote ,show-hint-symbol)))))

(provide 'keymap-hint)
