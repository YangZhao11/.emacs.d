;;; keymap-hint --- Show hint for keymap -*- lexical-binding: t -*-

;;; Commentary:
;;; This is somewhat similar to hydra, but decouping showing the hint
;;; and defining the keymap, which we do not handle. We do provide
;;; option to load the keymap using `set-transient-map'.

;;; Code:
;; TODO: maybe display-buffer with some action / hint would be enough.
(require 'lv)
(eval-when-compile
  (require 'cl-macs))

(defvar keymap-hint--stack nil
  "Stack of hint symbols we are showing.")

;; Keep a consistent state (minibuffer-depth). post-command-hook will
;; trigger before the whole command finishes, in the case of recursive
;; edit (completing read).
(defvar keymap-hint--show-top-pending nil)

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
  "Hide current hint. Re-trigger top of `keymap-hint--stack'."
  (interactive)
  (pop keymap-hint--stack)
  (lv-delete-window)
  (when keymap-hint--stack
    ;; We need to remember some state to re-show the top, and respect
    ;; this state in `keymap-hint-show'. Note post-command-hook will
    ;; trigger when the command calls `recursive-edit', e.g. in
    ;; `completing-read'. We want to show the top of stack after that
    ;; command finishes, so we remember minibuffer-depth here. Not
    ;; using recursion-depth for actual recursive edit.
    (setq keymap-hint--show-top-pending (minibuffer-depth))
    (add-hook 'post-command-hook #'keymap-hint--show-top-once)
    ))

(defun keymap-hint-cancel ()
  "Cancel the whole hint stack."
  (interactive)
  (setq keymap-hint--stack nil)
  (lv-delete-window))

(defvar-keymap keymap-hint-transient-map
  "SPC" #'keymap-hint-hide)

(defvar keymap-hint-cancel-commands
  '(switch-buffer
    display-buffer
    quit-window
    other-frame ; this triggers when going into this buffer?
    delete-other-windows))

(defun keymap-hint--should-cancel (command)
  "Returns non-nil if COMMAND should cancal hints."
  (if (symbolp command)
      (or (memq (or (get command 'command-semantic) command)
                keymap-hint-cancel-commands)
          ;; major mode change commands.
          (get command 'derived-mode-parent))
    ;; Don't know how to detect anything for lambda commands.
    ))

(defun keymap-hint--keep-hint ()
  "Decide if we keep the transient map."
  (cond
   ;; this-command might be a lambda.
   ((not (symbolp this-command)) 't)
   ((memq this-command '(keymap-hint-hide keymap-hint-cancel)) nil)
   ((eq (get this-command 'command-semantic) 'keymap-hint-show) nil)
   ((keymap-hint--should-cancel this-command)
    (keymap-hint-cancel)
    nil)
   (:else 't)))

(defun keymap-hint--keep-once ()
  "Always return nil, but hide the hint accordingly."
  (cond ((keymap-hint--should-cancel this-command)
         (keymap-hint-cancel))
        ((not (or (eq (get this-command 'command-semantic) 'keymap-hint-show)
                  (eq this-command 'keymap-hint-show)))
         ;; do not hide for `keymap-hint-show', otherwise it'll show it again.
         (keymap-hint-hide)))
  nil)

(defun keymap-hint--show-top ()
  "Show hint for top of `keymap-hint--stack'."
  (setq keymap-hint--show-top-pending nil)
  (let* ((keymap-symbol (car keymap-hint--stack))
         (prop (get keymap-symbol 'hint))
         (hint (plist-get prop :hint))
         (keep (plist-get prop :keep))
         (load-map (plist-get prop :load-map)))
    (when hint
      (cond ((or (and (symbolp hint) (fboundp hint))
                 (and (listp hint) (eq (car hint) 'lambda)))
             (setq hint (funcall hint)))
            ((listp hint)               ; all other forms
             (setq hint (eval hint 't))))
        (lv-message "%s" hint)
        (set-transient-map
         (cond ((keymapp load-map)
                load-map)
               ((and (symbolp load-map)
                     (boundp load-map)
                     (keymapp (symbol-value load-map)))
                (symbol-value load-map))
               ;; for other non-nil values, we load the
               (load-map
                (symbol-value keymap-symbol))
               ('t
                keymap-hint-transient-map))
         (if (eq keep 'once)
             'keymap-hint--keep-once
           'keymap-hint--keep-hint)
         ))))

(defun keymap-hint--show-top-once ()
  (if (null keymap-hint--show-top-pending)
      (remove-hook 'post-command-hook #'keymap-hint--show-top-once)
    (when (<= (minibuffer-depth) keymap-hint--show-top-pending)
      (remove-hook 'post-command-hook #'keymap-hint--show-top-once)
      (keymap-hint--show-top))))

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
  (if (and (eq keymap-symbol (car keymap-hint--stack))
           (not keymap-hint--show-top-pending))
      (keymap-hint-hide)
    :else
    (unless (eq keymap-symbol (car keymap-hint--stack))
      (push keymap-symbol keymap-hint--stack))
    (keymap-hint--show-top)))

(defun keymap-hint--format-string (hint)
  "Format string HINT to what we store in the hint property."
  (setq hint (string-trim hint))
  (setq hint (replace-regexp-in-string "·" "" hint))
  (setq hint (propertize-regexp
              hint "_\\([^_]+\\)_" 'face 'font-lock-function-name-face))
  hint)

(defun keymap-hint--format (hint)
  "Format HINT to what we store in the hint property."
  (cond ((stringp hint)
         (keymap-hint--format-string hint))
        ((and (listp hint)
              (eq (car hint) 'format)
              (stringp (cadr hint)))
         (setq hint (purecopy hint))
         (setf (cadr hint) (keymap-hint--format-string (cadr hint)))
         hint)
        ('t
         hint)))

;;;###autoload
(cl-defmacro keymap-hint-set (keymap hint &key bind load-map keep)
  "Set HINT for KEYMAP, which is a symbol of keymap name.

If HINT is a string or format call, we process _X_ constructs to add
highlight, and remove `·' to allow better aligning.

BIND specifies a key in KEYMAP to bind to show HINT function.

If LOAD-MAP is a keymap or symbol of a keymap, it is loaded. For other
non-nil value, KEYMAP is loaded after showing hint.

If KEEP is the symbol `once', the keymap is disabled after one command."
  (setq hint (keymap-hint--format hint))

  (let ((show-hint-symbol
         (intern (concat (symbol-name keymap) "-hint")))
        (hint-prop `(list :hint ,(if (listp hint) `(quote ,hint) hint)
                          :load-map ,load-map
                          :keep ,keep)))
    `(progn
       (put (quote ,keymap) 'hint ,hint-prop)
       (defun ,show-hint-symbol ()
         ,(concat "Show hint for `" (symbol-name keymap) "'.")
         (interactive)
         (keymap-hint-show (quote ,keymap)))
       (put (quote ,show-hint-symbol) 'command-semantic 'keymap-hint-show)
       ,(if bind
            `(keymap-set ,keymap ,bind (quote ,show-hint-symbol))))))

;;;###autoload
(defun keymap-hint-load-map-set (keymap-symbol key definition &optional remove)
  "Bind KEY for the load-map of KEYMAP-SYMBOL to DEFINITION.

 The load-map is the map loaded when hint for the keymap is shown."
  (let* ((prop (get keymap-symbol 'hint))
         (m (plist-get prop :load-map))
         new-map)
    (unless (keymapp m)
      (setq new-map (make-sparse-keymap))
      (cond
       ((and (symbolp m) (boundp m) (keymapp (symbol-value m)))
        (set-keymap-parent new-map (symbol-value m)))
       (m
        (set-keymap-parent new-map (symbol-value keymap-symbol))))
      (setq m new-map)
      (plist-put prop :load-map m))
    (define-key m key definition remove)))

(cl-defmacro keymap-hint-set-sub
    (keymap key hint &key bind symbol)
  "Set HINT for a sub map on KEY in KEYMAP.

SYMBOL specifies a symbol for the sub-keymap; if unspecified we
automatically generate one. The rest of the arguments are same as
`keymap-hint-set'."
  (unless symbol
    (let* ((parent-map-name (string-trim-right (symbol-name keymap) "-map"))
           (this-map-name (concat parent-map-name "-" key "-map")))
      (setq symbol (intern this-map-name))))
  (let ((hint-command-symbol
         (intern (concat (symbol-name symbol) "-hint"))))
    `(progn
       (defvar ,symbol (lookup-key ,keymap ,key))
       (keymap-hint-set ,symbol ,hint
                        :bind ,bind :load-map 't :keep 'once)
       (keymap-hint-load-map-set
        (quote ,keymap) ,key (quote ,hint-command-symbol)))))


(provide 'keymap-hint)
;;; keymap-hint.el ends here
