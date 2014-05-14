;;; project.el --- Handle projects -*- lexical-binding: t -*-

;;; Commentary:

;; Functions and key bindings that are useful when working on a
;; project spanning many directories and sub-directories.

;;; Code:

(require 'grep)

(add-to-list 'grep-find-ignored-files "TAGS")
(add-to-list 'grep-find-ignored-directories "bower_components")
(add-to-list 'grep-find-ignored-directories "databases")
(add-to-list 'grep-find-ignored-directories "lib")
(add-to-list 'grep-find-ignored-directories "node_modules")
(add-to-list 'grep-find-ignored-directories "venv")

(defvar project-root-files
  '(".hg")
  "Project files that mark the root of a project.")

(defun strip-text-properties(text)
  (set-text-properties 0 (length text) nil text)
  text)

(defun project-rgrep (regexp)
  "Recursively grep for REGEXP in the project root directory."
  (interactive (list
                (read-string "Search for: "
                             (strip-text-properties (thing-at-point 'symbol)))))
  (grep-compute-defaults)
  (rgrep regexp "*" (or (project-root) default-directory)))

(defun project-compile-and-visit-tags-table ()
  "Compile TAGS file at the project ROOT directory."
  (interactive)
  (let ((root (project-root)))
    (when root
      (add-hook 'compilation-finish-functions #'project-visit-tags-table)
      (compile (format "ctags -e -R --exclude=updates --languages=PHP -o %s %s"
                       (concat root "TAGS") root)))))

(defun project-visit-tags-table (buffer string)
  "Tell tags commands to use tags table at the project root."
  (when (string= string "finished\n")
    (visit-tags-table (concat (project-root) "TAGS")))
  (remove-hook 'compilation-finish-functions #'project-visit-tags-table))

(defun project-root ()
  "Return the project's root directory."
  (project-locate-first-dominating-file project-root-files))

(defun project-name ()
  "Return the project name determined by the root directory."
  (let ((root (project-root)))
    (when root
      (file-name-nondirectory (directory-file-name root)))))

(defun project-locate-first-dominating-file (files)
  "Return the first directory containing a file in FILES."
  (when files
    (or (locate-dominating-file default-directory (car files))
        (project-locate-first-dominating-file (cdr files)))))

(defun goto-django ()
  "Open dired buffer of the installed Django."
  (interactive)
  (dired (concat "~/.virtualenvs/"
                 (project-name)
                 "/lib/python2.7/site-packages/django")))

;;; Key bindings:

(global-set-key (kbd "C-c C-g") #'project-rgrep)
(global-set-key (kbd "<f11>") #'goto-django)

(provide 'project)
;;; project.el ends here
