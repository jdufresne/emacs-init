(defun rename-current-buffer-file (new-file-name)
  (interactive "FNew name: ")
  (let ((name (buffer-name))
        (file-name (buffer-file-name))
        (new-name (file-name-nondirectory new-file-name)))
    (if (not (and file-name (file-exists-p file-name)))
        (error "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (error "A buffer named '%s' already exists!" new-name)
        (rename-file file-name new-file-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-file-name)
        (set-buffer-modified-p nil)
        (message "File '%s' successfully renamed to '%s'" name new-name)))))


(defun delete-current-buffer-file ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (not (and file-name (file-exists-p file-name)))
        (kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (kill-buffer)
        (delete-file file-name)
        (message "File '%s' successfully removed" file-name)))))


(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Always kill the current buffer without asking
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer)))
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(provide 'current-buffer)
