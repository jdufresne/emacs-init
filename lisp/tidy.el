 ;;; -*- lexical-binding: t -*-

(defvar tidy-alist
  '(("\.json$" "python" "-m" "json.tool"))
  "An alist where the key is a regexp to match the buffer name
  and the value is a list of program and arguments used to tidy
  the buffer.")


;;; Code:

(defun tidy-buffer ()
  "Replace buffer with tidy version.

Returns t if buffer was successfully transformed; nil otherwise."
  (interactive)
  (barf-if-buffer-read-only)
  (let ((spec (assoc-regexp (buffer-name) tidy-alist)))
    (if (not spec)
        (error "No tidy spec found for buffer %s" (buffer-name))
      (let* ((output-file (make-temp-file "tidy"))
             (error-file (make-temp-file "tidy"))
             (exit-status (apply #'call-process-region
                                 (point-min) (point-max)
                                 (cadr spec)
                                 nil
                                 (list (list :file output-file) error-file)
                                 nil
                                 (cddr spec))))
        (if (= exit-status 0)
            (insert-file-contents output-file nil nil nil t)
          (with-current-buffer (get-buffer-create "*Tidy Error*")
            (goto-char (point-max))
            (insert-file-contents error-file)
            (display-buffer (current-buffer))))
        (delete-file output-file)
        (delete-file error-file)
        (= exit-status 0)))))


(defun assoc-regexp (string list)
  "Return non-nil if the car of an element of LIST matches in STRING.
The value is actually the first element of LIST whose car matches
in STRING."
  (if (not list)
      nil
    (if (string-match (caar list) string)
        (car list)
      (assoc-regexp string (cdr list)))))

(provide 'tidy)

;;; tidy.el ends here
