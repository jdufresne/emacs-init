;;; project.el --- Handle projects -*- lexical-binding: t -*-


;;; Code:

(defvar rs-buffer-name "*runserver*")

(defun rs-root ()
  (with-temp-buffer
    (let ((rc (call-process "hg" nil t nil "root")))
      (if (not (= rc 0))
          nil
        (let ((s (buffer-string)))
          (string-match "\n" s)
          (replace-match "" t t s))))))

(defun runserver ()
  (interactive)
  (let ((root (rs-root)))
    (if (not root)
        (error "No repository found")
      (let ((b (get-buffer rs-buffer-name))
            (kill-buffer-query-functions nil))
        (when b
          (kill-buffer b)))
      (setenv "SITE_CONFIG" (concat root "/erezlife"))
      (let ((branch (file-name-base root)))
        (start-process "php-web-server" rs-buffer-name
                       "php"
                       "-S" "127.0.0.1:8080"
                       "-t" (concat root "/legacy"))
        (start-process "py-web-server" rs-buffer-name
                       (concat "~/.virtualenvs/" branch "/bin/python")
                       (concat root "/manage.py")
                       "runserver"))
      (with-current-buffer rs-buffer-name
        (term-mode))
      (display-buffer rs-buffer-name))))

(global-set-key (kbd "<f10>") #'runserver)

(provide 'runserver)
