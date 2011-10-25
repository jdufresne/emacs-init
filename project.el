(defun project-root-dir ()
  (let ((root-dir (pwd)))
	(while (not (file-exists-p (concat root-dir ".hg")))
	  (setq root-dir (file-name-as-directory (concat root-dir ".."))))
	(file-name-as-directory (expand-file-name root-dir))))


(defun project-compile-tags-table ()
  (interactive)
  (compile (mapconcat 'identity
					  (list "ctags" "-e" "-R" "--languages=PHP"
							"-o" (concat (project-root-dir) "TAGS")
							(project-root-dir))
					  " ")))

(defun project-visit-tags-table ()
  (interactive)
  (visit-tags-table (concat (project-root-dir) "TAGS")))


(defun save-all-buffers ()
  (save-some-buffers t))


(provide 'project)
