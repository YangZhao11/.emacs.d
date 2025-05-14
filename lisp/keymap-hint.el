;;; keymap-hint --- Show hint for keymap -*- lexical-binding: t -*-

;;; Commentary:
;;; This is somewhat similar to hydra, but decouping showing the hint
;;; and defining the keymap, which we do not handle. We do provide
;;; option to load the keymap using `set-transient-map'.

;;; Code:
;; TODO: maybe display-buffer with some action / hint would be enough.
(require 'lv)

;; TODO: make this a stack, so that we can do layered hint and return
;; to previous state.
(defvar keymap-hint--current nil
  "Current hint that is showing.")

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
   ((not (symbolp this-command)) 't)
   ((eq this-command 'keymap-hint-hide) nil)
   ((eq (get this-command 'command-semantic) 'keymap-hint-show) nil)
   ((or (memq (get this-command 'command-semantic)
              '(switch-buffer quit-window))
        ;; major mode change commands
        (get this-command 'derived-mode-parent))
    (keymap-hint-hide)
    nil)
   (:else 't)))

(defun keymap-hint--keep-once ()
  "Decide if we keep the transient map."
  (keymap-hint-hide)
  nil)

;;;###autoload
(defun keymap-hint-show (keymap-symbol)
  "Show mode hint attached to KEYMAP-SYMBOL if present.

Look up the hint property from keymap symbol; it is a list of
format (hint load). hint is a string or a form to be evaluated.
If load is non-nil we load the keymap. If load is the symbol once,
the keymap is deactivated after one command."
  (interactive)
  (unless (and (symbolp keymap-symbol)
               (keymapp (symbol-value keymap-symbol)))
    (error "Keymap symbol expected"))
  (if (eq keymap-symbol keymap-hint--current)
      (keymap-hint-hide)
    :else
    (setq keymap-hint--current keymap-symbol)
    (let* ((prop (get keymap-symbol 'hint))
                (hint (car prop))
                (load (cadr prop)))
      (when hint
        (if (listp hint)
          (setq hint (eval hint 't)))

        (lv-message hint)
        (set-transient-map
         (if load
             (symbol-value keymap-symbol)
           keymap-hint-transient-map)
         (if (eq load 'once)
             'keymap-hint--keep-once
           'keymap-hint--keep-hint)
         )))))

;;;###autoload
(defmacro keymap-hint-set (keymap key hint &optional load)
  ;; TODO: parse hint to format form if it contains %() constructs.
  ;; TODO: add keymap setting, we will build a keymap that
  ;; parent to KEYMAP.
  "Set HINT for KEYMAP, which is a symbol of keymap name.

Bind KEY in KEYMAP to show HINT. If LOAD is non-nil, the keymap is
loaded after showing hint. If LOAD is the symbol `once', the keymap is
disabled after one command."
  ;;(and (symbolp keymap) (keymapp (symbol-value keymap)))
  (setq hint (string-trim hint))
  (setq hint (replace-regexp-in-string "·" "" hint))
  (setq hint (propertize-regexp
              hint "_\\([^_]+\\)_" 'face 'font-lock-function-name-face))

  (let ((show-hint-symbol
         (intern (concat (symbol-name keymap) "-hint"))))
    `(progn
       (put (quote ,keymap) 'hint '(,hint ,load))
       (defun ,show-hint-symbol ()
         ,(concat "Show hint for `" (symbol-name keymap) "'.")
         (interactive)
         (keymap-hint-show (quote ,keymap)))
       (put (quote ,show-hint-symbol) 'command-semantic 'keymap-hint-show)
       ,(if key
            `(keymap-set ,keymap ,key (quote ,show-hint-symbol))))))

(provide 'keymap-hint)
;;; keymap-hint.el ends here
