(require 'flymake)

(defun flymake-csslint-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-inplace t t
   (file-name-nondirectory buffer-file-name)
   'flymake-get-csslint-cmdline))

(defun flymake-get-csslint-cmdline (source base-dir)
  (list "rhino" (list (expand-file-name "~/.emacs.d/js/csslint.js")
					  "--format=compact"
					  (concat "--rules="
							  (mapconcat 'identity
										 '("empty_rules"
										   "display-property-grouping"
										   "zero-units"
										   "vendor-prefix"
										   "gradient"
										   "regex-selectors"
										   "import"
										   "important"
										   "compatible-vendor-prefixes"
										   "duplicate-properties")
										 ","))
					  (concat base-dir source))))

(push '("\\.css$" flymake-csslint-init)
	  flymake-allowed-file-name-masks)
(push '("^\\(.+\\): line \\([0-9]+\\), col \\([0-9]+\\), \\(.*\\)$" 1 2 3 4)
	  flymake-err-line-patterns)

(add-hook 'css-mode-hook
		  (lambda () (flymake-mode t)))

(provide 'flymake-csslint)