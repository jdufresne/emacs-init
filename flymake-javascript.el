(require 'flymake)

(defun flymake-javascript-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-inplace t t
   (file-name-nondirectory buffer-file-name)
   'flymake-get-javascript-cmdline))

(defun flymake-get-javascript-cmdline (source base-dir)
  (list "js" (list "-s" "-C" (concat base-dir source))))

(push '("\\.js$" flymake-javascript-init)
	  flymake-allowed-file-name-masks)
(push '("\\.json$" flymake-javascript-init)
	  flymake-allowed-file-name-masks)
(push '("^\\(.+\\):\\([0-9]+\\): \\(.+\\):$" 1 2 nil 3)
	  flymake-err-line-patterns)

(add-hook 'js-mode-hook
		  (lambda () (flymake-mode t)))

(provide 'flymake-javascript)