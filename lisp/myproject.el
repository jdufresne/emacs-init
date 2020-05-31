;;; project.el --- Handle projects -*- lexical-binding: t -*-

;;; Commentary:

;; Functions and key bindings that are useful when working on a
;; project spanning many directories and sub-directories.

;;; Code:

(require 'python)
(require 's)
(require 'vc-git)

(defun project-root ()
  (vc-git-root default-directory))

(defun project-expand-root (name)
  (expand-file-name name (project-root)))

(defun python-version ()
  (with-temp-buffer
    (call-process python-shell-interpreter nil t nil
                  "-c" "import sys; print('%d.%d' % sys.version_info[:2])")
    (s-trim (buffer-string))))

(defun goto-django ()
  "Open dired buffer of the installed Django."
  (interactive)
  (dired (project-expand-root (concat "venv/lib/python"
                                      (python-version)
                                      "/site-packages/django"))))

(defun run-development-server (buffer-name make-target)
  "Run development server.

Server is starting by running make target MAKE-TARGET in the
project's root directory. The development server's output will
appear in buffer BUFFER-NAME."
  (let ((default-directory (project-root))
        (compilation-read-command nil)
        (compilation-buffer-name-function (lambda (name-of-mode) buffer-name))
        (project-name (file-name-nondirectory (directory-file-name (project-root)))))
    (compile (concat "make EREZLIFE_CONFIG_HOST=localhost EREZLIFE_CONFIG_KEY=" project-name " " make-target) t))
  (buffer-disable-undo buffer-name))


(defun run-django ()
  "Run Django development server."
  (interactive)
  (run-development-server "*Django*" "run-django"))

(defun run-php ()
  "Run PHP development server."
  (interactive)
  (run-development-server "*PHP*" "run-php"))

(defun run-webpack ()
  "Run webpack development server."
  (interactive)
  (run-development-server "*webpack*" "run-webpack"))

(defun run-development-servers ()
  "Run all development servers."
  (interactive)
  (kill-development-servers)
  (run-webpack)
  (run-php)
  (run-django))

(defun kill-buffer-if-exists (buffer-name)
  "Kill buffer named BUFFER-NAME if it exists."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (kill-buffer buffer))))

(defun kill-development-servers ()
  "Kill all development server buffers."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (dolist (buffer-name '("*webpack*" "*PHP*" "*Django*"))
      (kill-buffer-if-exists buffer-name))))

;;; Key bindings:

(global-set-key (kbd "<f10>") #'goto-django)
(global-set-key (kbd "<f5>") #'run-development-servers)
(global-set-key (kbd "S-<f5>") #'kill-development-servers)

(provide 'myproject)
;;; myproject.el ends here
