;;;; emacs init file
;; Jon Dufresne <jon@jondufresne.org>


;; Basic config
(setq inhibit-splash-screen t)
(setq frame-title-format '(buffer-file-name "%f" "%b"))
(setq default-frame-alist '((font . "Inconsolata 12")))
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
(blink-cursor-mode t)


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

(add-hook 'sql-interactive-mode-hook
		  (lambda () (linum-mode 0)))

(add-hook 'shell-mode-hook
		  (lambda () (linum-mode 0)))

(setq whitespace-style '(empty trailing))
(add-hook 'before-save-hook 'whitespace-cleanup)


;; Load from external files
(add-to-list 'load-path "~/.emacs.d/")

;; Handle in external files
(require 'keys)
(require 'smart-lines)
(require 'unfill)
(require 'project)

(require 'flymake-javascript)
(require 'flymake-csslint)


;; libs
(require 'smarttabs)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'hl-tags-mode)
(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(require 'grep-a-lot)
(grep-a-lot-setup-keys)


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
  (-safe-indent-region-rigidly (- tab-width)))

(global-set-key (kbd "C-<tab>") 'indent-rigidly-tab-width)
(global-set-key (kbd "C-<iso-lefttab>") 'indent-rigidly-tab-width)
(global-set-key (kbd "C-S-<tab>") 'dedent-rigidly-tab-width)
(global-set-key (kbd "C-S-<iso-lefttab>") 'dedent-rigidly-tab-width)
