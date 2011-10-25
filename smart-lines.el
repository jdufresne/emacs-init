(defun smart-beginning-of-line ()
  (interactive)
  (let ((pos (point)))
	(back-to-indentation)
	(if (= pos (point))
		(beginning-of-line))))

(defun smart-end-of-line ()
  (interactive)
  (let ((pos (point)))
	(end-of-line)
	(re-search-backward "[^ \t]" (line-end-position 0) t)
	(forward-char)
	(if (= pos (point))
		(end-of-line))))

(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(global-set-key (kbd "<end>") 'smart-end-of-line)
(global-set-key (kbd "C-e") 'smart-end-of-line)

(provide 'smart-lines)