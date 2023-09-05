;;; z-misc -- Misc defuns -*- lexical-binding: t -*-
;;; Commentary:
;;  Miscellaneous functions that is not always used.

;;; Code:

(defvar z-align-alist
  '((?  "\\s-" 0)
    (?\" "\\s-\"" 0))
  "Specify regexp and spacing for z-align-char.")

(defun z-align-char (char beg end no-space)
  "Align CHAR in region specified by BEG and END.
With prefix arg (NO-SPACE), do not leave space before CHAR."
  (interactive "cAlign char: \nr\nP")
  (let* ((a (alist-get char z-align-alist))
         (regexp (concat  "\\(\\s-*\\)"
                          (if a (car a)
                            (regexp-quote (char-to-string char)))))
         (spacing (if a (cadr a)
                    (if no-space 0 1))))
    (align-regexp beg end regexp 1 spacing t)))

(defun z-toggle-selective-display (column)
  "Toggle selective display at COLUMN, defaulting to current column."
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun z-ediff-this-buffer ()
  "Call ediff on this buffer, with the version on disk or backup."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not associated with a file"))
  (if (buffer-modified-p)
      (ediff-current-file)
    (ediff-backup (buffer-file-name))))

(defun z-shrink-other-window-if-larger-than-buffer ()
    "Shrink other window if larger than buffer."
    (interactive)
    (shrink-window-if-larger-than-buffer
     (next-window (selected-window) nil nil)))

(defun z-prev-buffer-next-window ()
  "Switch other window to previous buffer."
  (interactive)
  (switch-to-prev-buffer (next-window)))
(defun z-next-buffer-next-window ()
  "Switch other window to next buffer."
  (interactive)
  (switch-to-next-buffer (next-window)))

(defvar z-search-char nil
  "Last char used in `z-search-forward-char'")
(defvar z-search-bound-lines 5
  "Limit number of lines to search")
(defun z-search-forward-char (arg char)
  "Search for next CHAR."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "char: " t)))
  (setq z-search-char char)
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
	(search-forward
         (char-to-string char)
         (and z-search-bound-lines
              (line-end-position z-search-bound-lines))
         nil arg)
      (backward-char direction))))

(defun z-search-backward-char (arg char)
  "Search for previous CHAR."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "char: " t)))
  (setq z-search-char char)
  (search-backward
   (char-to-string char)
   (and z-search-bound-lines
        (line-beginning-position (- z-search-bound-lines)))
   nil arg))

(defun z-search-forward-repeat (arg)
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (z-search-forward-char arg z-search-char))

(defun z-search-backward-repeat (arg)
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (z-search-backward-char arg z-search-char))

(defvar-keymap z-search-repeat-map
  :repeat t
  "." #'z-search-forward-repeat
  "," #'z-search-backward-repeat)
(put 'z-search-forward-char 'repeat-map 'z-search-repeat-map)
(put 'z-search-backward-char 'repeat-map 'z-search-repeat-map)

(provide 'z-misc)
;;; z-misc.el ends here
