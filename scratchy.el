;;; package --- scratch buffers with different major modes
;;;

;;; Commentary:
;;; scratch buffers with different major modes
;;;
;;;
;;;

;;; Code:

(defvar scratchy-map (make-hash-table)
  "This maps modes to buffers.  One buffer for each mode.")

(defun scratchy-update-map (map)
  "Expect MAP to be scratchy mapping from mode to buffer.
Return list with existing buffers only."
  (let ((updated-list (make-hash-table)))
    (progn
      (dolist (mode (map-keys map))
        (let ((buffer (gethash mode map)))
          (when (bufferp buffer)
            (puthash mode buffer map))))
      updated-list)))

(defun scratchy-goto-mode (mode)
  "Get the scratchy buffer for MODE.
Display it in a newly created window in a newly created frame."
  (interactive)
  (let ((scratchy-buffer (gethash mode scratchy-map)))
    (when (or
           (not (buffer-live-p scratchy-buffer))
           (null scratchy-buffer))
      (progn
        (setq scratchy-buffer (generate-new-buffer (format "*scratchy-%s*" (symbol-name mode))))
        (with-current-buffer scratchy-buffer
          (funcall mode))
        (puthash mode scratchy-buffer scratchy-map)))
    (select-frame (make-frame))
    (switch-to-buffer scratchy-buffer)))

(defconst scratchy-modes-list '("text-mode" "python-mode" "emacs-lisp-mode" "java-mode" "org-mode" "sh-mode" "json-mode" "sql-mode"))

(defun scratchy-open-buffer ()
  "Open a scratch buffer for a specific mode."
  (interactive)
  (let ((mode-of-choice (completing-read "Scratch-buffer for Which mode? " scratchy-modes-list)))
    (scratchy-goto-mode (intern mode-of-choice))))

(global-set-key (kbd "C-c s") 'scratchy-open-buffer)

(provide 'scratchy)
