(defvar *json-tidy-indent* 4)


(defun json-tidy-newline (level)
  (newline)
  (insert-char ?\ (* level *json-tidy-indent*)))


(defun json-tidy-walk ()
  (let ((level 0)
		(quote nil)
		(escape nil))
	(while (not (eobp))
	  (if quote
		  (progn
			(if (and (not escape) (char-equal ?\" (following-char)))
				(setq quote nil)
			  (setq escape (char-equal ?\\ (following-char))))
			(forward-char))

		(let ((delete nil)
			  (nl nil)
			  (space nil))
		  (cond ((char-equal ?\" (following-char))
				 (setq quote t))
				((or (char-equal ?\{ (following-char))
					 (char-equal ?\[ (following-char)))
				 (setq nl t)
				 (setq level (1+ level)))
				((or (char-equal ?\} (following-char))
					 (char-equal ?\] (following-char)))
				 (setq level (1- level))
				 (json-tidy-newline level)
				 (setq nl t))
				((char-equal ?\, (following-char))
				 (setq nl t))
				((char-equal ?\: (following-char))
				 (setq space t))
				((looking-at "[ \t\r\n]")
				 (setq delete t)))

		  (if delete
			  (delete-char 1)
			(forward-char)
			(when nl
			  (json-tidy-newline level))
			(when space
			  (insert " "))))))))


(defun json-tidy ()
  (interactive)
  (goto-char (point-min))
  (json-tidy-walk)
  (goto-char (point-min)))


(provide 'json-tidy)
