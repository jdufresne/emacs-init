(defun bounds-of-region-or-thing (thing)
  (if (use-region-p)
	  (cons (region-beginning) (region-end))
	(bounds-of-thing-at-point thing)))


(defun -indent-region-rigidly (count)
  (let ((bounds (bounds-of-region-or-thing 'line)))
	(indent-rigidly (car bounds) (cdr bounds) count)))


(defun indent-rigidly-tab-width ()
  (interactive)
  (-indent-region-rigidly tab-width))


(defun dedent-rigidly-tab-width ()
  (interactive)
  (-indent-region-rigidly (- tab-width)))


(global-set-key (kbd "C-<tab>") 'indent-rigidly-tab-width)
(global-set-key (kbd "C-<iso-lefttab>") 'indent-rigidly-tab-width)
(global-set-key (kbd "C-S-<tab>") 'dedent-rigidly-tab-width)
(global-set-key (kbd "C-S-<iso-lefttab>") 'dedent-rigidly-tab-width)


(provide 'region-rigidly)
