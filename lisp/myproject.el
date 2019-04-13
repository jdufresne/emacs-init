;;; project.el --- Handle projects -*- lexical-binding: t -*-

;;; Commentary:

;; Functions and key bindings that are useful when working on a
;; project spanning many directories and sub-directories.

;;; Code:

(require 's)
(require 'projectile)

(defun python-version ()
  (with-temp-buffer
    (call-process "python3" nil t nil
                  "-c" "import sys; print('%d.%d' % sys.version_info[:2])")
    (s-trim (buffer-string))))

(defun goto-django ()
  "Open dired buffer of the installed Django."
  (interactive)
  (dired (projectile-expand-root (concat
                                  "venv/lib/python" (python-version) "/site-packages/django"))))

;;; Key bindings:

(global-set-key (kbd "<f10>") #'goto-django)

(provide 'myproject)
;;; myproject.el ends here
