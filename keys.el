;;; Keys

(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-x C-g") 'grep)
(global-set-key (kbd "<f11>") 'shell)
(global-set-key (kbd "<f12>") 'sql-mysql)

;; Remove annoying keys
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(provide 'keys)
