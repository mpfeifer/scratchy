(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*"))

(defvar scratch-map (make-hash-table))

(defun scratch-update-map (map)
  (let ((updated-list (make-hash-table)))
    (progn
      (dolist (mode (map-keys map))
        (let ((buffer (gethash mode map)))
          (when (bufferp buffer)
            (puthash mode buffer map))))
      updated-list)))

(defun scratch-goto-mode (mode)
  (interactive)
  (let ((scratch-buffer (gethash mode scratch-map)))
    (when (or
           (not (buffer-live-p scratch-buffer))
           (null scratch-buffer))
      (progn
        (setq scratch-buffer (generate-new-buffer (format "*scratch-%s*" (symbol-to-string mode))))
        (with-current-buffer scratch-buffer
          (funcall mode))
        (puthash mode scratch-buffer scratch-map)))
    (select-frame (make-frame))
    (switch-to-buffer scratch-buffer)))

(defun scratch-goto-text-mode ()
  (interactive)
  (scratch-goto-mode 'text-mode))

(defun scratch-goto-emacs-lisp-mode ()
  (interactive)
  (scratch-goto-mode 'emacs-lisp-mode))

(defun scratch-goto-python-mode ()
  (interactive)
  (scratch-goto-mode 'python-mode))

(defun scratch-goto-java-mode ()
  (interactive)
  (scratch-goto-mode 'java-mode))

(defun scratch-goto-org-mode ()
  (interactive)
  (scratch-goto-mode 'org-mode))

(defun scratch-goto-shl-mode ()
  (interactive)
  (scratch-goto-mode 'sh-mode))

(defhydra hydra-global-scratch (:color blue :hint nil)
  "
Mode^^
----------------------------------------------
_t_ext-mode    _p_ython-mode        _o_rg-mode
_j_ava-mode    emacs-_l_isp-mode    _s_h-mode
"
  ("t" scratch-goto-text-mode)
  ("l" scratch-goto-emacs-lisp-mode)
  ("p" scratch-goto-python-mode)
  ("j" scratch-goto-java-mode)
  ("o" scratch-goto-org-mode)
  ("s" scratch-goto-shl-mode))

(global-set-key (kbd "C-c s") 'hydra-global-scratch/body)


~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
~
69 fewer lines; before #2  7 seconds ago

