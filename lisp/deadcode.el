;; replaced with ace-window
(global-set-key (kbd "M-9") (lambda () (interactive)
                              (other-window -1)))
(global-set-key (kbd "M-0") 'other-window)
(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(global-set-key (kbd "M-(") (lambda () (interactive)
                              (transpose-windows -1)))
(global-set-key (kbd "M-)") 'transpose-windows)

;; Set frame font to "Monospace-nn", where nn is the key pressed last
;; with a lookup in font-size-by-digit. That is, if invoked by "C-x
;; C-1" frame font will be set to "Monospace-8".
(setq font-size-by-digit [30 8 9 10 11 12 14 16 19 24])
(setq frame-font-name-format "Liberation Mono-%d")
(defun set-frame-font-size-self-digit (&optional arg)
  (interactive "p")
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0))
         (font-size (if (and (> arg 5) (<= arg 30)) arg
                      (elt font-size-by-digit digit)))
         (font-name (format frame-font-name-format font-size)))
    (set-frame-font font-name t)
    (message "Frame font set to %s" font-name)))

(defun toggle-variable (var)
  "toggles boolean variable"
  (interactive "vToggle variable: ")
  (set var (not (symbol-value var)))
  (message "%s set to %s" var (symbol-value var)))

(setq preferred-debugger-alist
      '((c-mode . gdb)
        (c++-mode . gdb)
        (cperl-mode . perldb)
        (python-mode . pdb)
        (jde-mode . jdb)))
(defun z-maybe-gud ()
  "Run gdb if not already running, otherwise bring it to front"
  (interactive)
  (require 'gud)
  (if (and (boundp 'gud-comint-buffer)  ;find running gdb process
           gud-comint-buffer
           (buffer-name gud-comint-buffer)
           (get-buffer-process gud-comint-buffer))
      (if (fboundp 'gdb-restore-windows)
           (gdb-restore-windows)
        (pop-to-buffer gud-comint-buffer))
    (call-interactively
     (or (cdr (assq major-mode preferred-debugger-alist))
         'gdb))))
(global-set-key (kbd "C-x <f8>") 'z-maybe-gud)

(defun z-maybe-recompile (&optional arg)
  "recompile if possible"
  (interactive "P")
  (if (and (fboundp 'recompile) (not arg))
      (recompile)
    (call-interactively 'compile)))
(global-set-key [f5] 'z-maybe-recompile)
