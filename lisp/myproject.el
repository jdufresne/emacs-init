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

(defun file-path-to-python-path (path)
  (s-join "."
          (s-split "/"
                   (s-chop-prefix (projectile-project-root) path))))

(defun project-test-django-extra-args ()
  "Return default arguments to pass to Django tests."
  (if (and buffer-file-name
           (string-match "\\(.*tests\\)\.py$" buffer-file-name))
      (concat " " (file-path-to-python-path (match-string 1 buffer-file-name)))
    ""))

(defun project-test-django ()
  "Run Django tests."
  (interactive)
  (let ((default-directory (projectile-project-root))
        (compilation-scroll-output t)
        (compile-command (concat
                          "venv/bin/python manage.py test -v 2 --noinput"
                          (project-test-django-extra-args))))
    (call-interactively #'compile)))

(defun php-test-php-extra-args ()
  "Return default arguments to pass to phpunit."
  (if (and buffer-file-name
           (string-match "/\\([[:alnum:]]+Test\\)\.php$" buffer-file-name))
      (concat " " (match-string 1 buffer-file-name) " " buffer-file-name)
    ""))

(defun project-test-php ()
  "Run PHP tests."
  (interactive)
  (let ((phpunit (projectile-expand-root "legacy/vendor/bin/phpunit"))
        (phpunit-xml (projectile-expand-root "legacy/phpunit.xml"))
        (compilation-scroll-output t))
    (let ((compile-command (concat phpunit " --debug -c " phpunit-xml (php-test-php-extra-args))))
      (call-interactively #'compile))))

(add-to-list 'compilation-error-regexp-alist 'php)
(add-to-list 'compilation-error-regexp-alist-alist
             '(php "^\\([^ \n]+\\):\\([0-9]+\\)$" 1 2))

;;; Key bindings:

(global-set-key (kbd "<f10>") #'goto-django)
(global-set-key (kbd "<f6>") #'project-test-django)
(global-set-key (kbd "<f7>") #'project-test-php)

(provide 'myproject)
;;; project.el ends here
