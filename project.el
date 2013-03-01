;;; project.el --- Handle projects

;;; Commentary:

;; Functions and key bindings that are useful when working on a
;; project spanning many directories and sub-directories.

;;; Code:

(defvar project-root-files
  '(".hg")
  "Project files that mark the root of a project.")

(defun project-rgrep (regexp)
  "Recursively grep for REGEXP in the project root directory."
  (interactive "sSearch for: ")
  (grep-compute-defaults)
  (rgrep regexp "*" (or (project-root) default-directory)))

(defun project-compile-tags-table (root)
  "Compile TAGS file at the project ROOT directory."
  (compile (format "ctags -e -R --languages=PHP -o %s %s"
                   (concat root "TAGS") root)))

(defun project-visit-tags-table ()
  "Tell tags commands to use tags table at the project root."
  (interactive)
  (let ((root (project-root)))
    (when root
      (project-compile-tags-table root)
      (visit-tags-table (concat root "TAGS")))))

(defun project-root ()
  "Return the root directory of the project."
  (project-locate-first-dominating-file project-root-files))

(defun project-locate-first-dominating-file (files)
  "Return the first directory containing a file in FILES."
  (when files
    (or (locate-dominating-file default-directory (car files))
        (project-locate-first-dominating-file (cdr files)))))

;;; Key bindings:

(global-set-key (kbd "C-c C-g") 'project-rgrep)

(provide 'project)
;;; project.el ends here
