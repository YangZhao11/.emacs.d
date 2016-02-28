;;; grab-region -- Grab selected region
;;; Commentary:
;;

;;; Code:

(defun grab-region-off ()
  "Turn off `grab-region-mode'."
  (interactive)
  (grab-region-mode -1))

(defvar grab-region-mode-map
  (make-sparse-keymap))

(define-minor-mode grab-region-mode
  "Grab the region.  Movement commands move the region as a whole."
  :lighter " Grab"
  :keymap grab-region-mode-map
  (cond ((not grab-region-mode)
         (remove-hook 'deactivate-mark-hook 'grab-region-off))
        ((use-region-p)
         (add-hook 'deactivate-mark-hook 'grab-region-off))
        ('t (message "No active region")
            (setq grab-region-mode nil))))


(defun grab-region-move (command &optional type)
  "Call COMMAND interactively, then move region to the point afterwards.
TYPE can be 'forward or 'backword."
  (let ((p (point)) (m (mark))
        (arg (cond ((integerp current-prefix-arg) current-prefix-arg)
                   ((eq current-prefix-arg '-) -1)
                   ('t 1)))
        (string) (new-begin))
    (when (< arg 0)
      (setq type (cond ((eq type 'forward) 'backward)
                       ((eq type 'backward) 'forward))))
    (when (or (and (eq type 'forward) (< p m))
              (and (eq type 'backward) (> p m)))
      (goto-char m))

    (call-interactively command)
    (setq string (filter-buffer-substring p m 'delete))
    (setq new-begin (point))
    (insert string)
    (if (< p m)
      (progn (set-mark (point))
             (goto-char new-begin))
      (set-mark new-begin))
    (activate-mark)
    (setq deactivate-mark nil)))

(defmacro grab-region-remap (command &optional type)
  "Remap COMMAND in grab region mode.
COMMAND should move the point.
TYPE sepcifies several possible formats:
    'forward means COMMAND takes integer arguments and positive arguments
move the point forward;
    'backward means COMMAND takes integer arguments and positive arguments
move the point backward.
     nil means the command does not take arguments."
  (let ((func (make-symbol (concat "grab-region/" (symbol-name command))))
        (doc (concat "Move cursor using `" (symbol-name command) "', adopted for grab-region.")))
    `(progn
       (defun ,func () ,doc
              (interactive)
              (grab-region-move (quote ,command) (quote ,type)))
       (define-key grab-region-mode-map [remap ,command] (quote ,func)))))

(grab-region-remap forward-char forward)
(grab-region-remap backward-char backward)

(grab-region-remap move-beginning-of-line backward)
(grab-region-remap move-end-of-line forward)

(grab-region-remap next-line forward)
(grab-region-remap previous-line backward)

(grab-region-remap forward-word forward)
(grab-region-remap backward-word backward)

(grab-region-remap forward-sentence forward)
(grab-region-remap backward-sentence backward)

(grab-region-remap forward-paragraph forward)
(grab-region-remap backward-paragraph backward)

(grab-region-remap forward-sexp forward)
(grab-region-remap backward-sexp backward)

(grab-region-remap beginning-of-defun backward)
(grab-region-remap end-of-defun forward)

(grab-region-remap backward-up-list backward)
(grab-region-remap down-list forward)

(provide 'grab-region)
;;; grab-region.el ends here
