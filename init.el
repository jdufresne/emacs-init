;;;; emacs init file
;; Jon Dufresne <jon@jondufresne.org>


;; Basic config
(setq inhibit-splash-screen t)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(set-frame-font "Inconsolata 12")
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-scroll-bar-mode 'right)
(prefer-coding-system 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq-default truncate-lines t)
(setq next-line-add-newlines nil)
(setq save-place-file "~/.emacs.d/emacs-places")
(setq-default save-place t)
(setq-default require-final-newline t)
(global-auto-complete-mode -1)


;; Fix copy-paste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)


;; Global minor modes
(global-linum-mode t)
(setq column-number-mode t)
(show-paren-mode t)
(global-hl-line-mode t)
(set-face-background 'hl-line "#ffffe0")
(delete-selection-mode t)
(global-auto-revert-mode t)


;; Enable functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(add-to-list 'same-window-buffer-names "*grep*")
(add-to-list 'same-window-buffer-names "*SQL*")


;; Style
(setq-default tab-width 4)
(setq nxml-child-indent 4)

(require 'cc-mode)
(defconst erez-c-style
  '((c-basic-offset . 4)
	(c-offsets-alist . ((arglist-close . 0)
						(substatement-open . 0)
						(case-label . +)))))

(c-add-style "erez" erez-c-style)
(setq c-default-style "erez")

;; Hooks
(add-hook 'text-mode-hook
		  (lambda () (flyspell-mode)))

(add-hook 'c-mode-common-hook
		  (lambda () (subword-mode t)))

(add-hook 'php-mode-hook
		  (lambda () (flymake-mode t)))

(require 'whitespace)
(setq whitespace-style '(empty trailing))
(add-hook 'before-save-hook 'whitespace-cleanup)


;; Load from external files
(add-to-list 'load-path "~/.emacs.d/")

;; Handle in external files
(require 'keys)
(require 'sudo)
(require 'smart-lines)
(require 'unfill)
(require 'project)

(require 'flymake-javascript)
(require 'flymake-csslint)


;; libs
(autoload 'php-mode "php-mode" "PHP editing mode" t nil)
(require 'smarttabs)
(require 'saveplace)

(require 'undo-tree)
(global-undo-tree-mode)


(require 'hl-tags-mode)
(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))
