;;;; emacs init file
;; Jon Dufresne <jon@jondufresne.org>


;; Basic config
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-splash-screen t)
(setq frame-title-format '(buffer-file-name "%f" "%b"))
(setq default-frame-alist '((font . "Inconsolata 12")))
(set-scroll-bar-mode 'right)
(prefer-coding-system 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-to-list 'backup-directory-alist '("^.*$" . "~/.local/share/emacs"))
(setq-default truncate-lines t)
(setq next-line-add-newlines nil)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.cache/emacs/places")
(setq-default require-final-newline t)
(blink-cursor-mode t)
;(setq-default indent-tabs-mode nil)
(setq next-screen-context-lines 4)

;; Fix copy-paste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Enable functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Global minor modes
(global-linum-mode t)
(setq column-number-mode t)
(show-paren-mode t)
(global-hl-line-mode t)
(set-face-background 'hl-line "#ffffe0")
(delete-selection-mode t)

;; Auto revert mode
(require 'autorevert)
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show in the current window
(add-to-list 'same-window-buffer-names "*grep*")
(add-to-list 'same-window-regexps "\\*grep\\*<[[:digit:]]+>")
(add-to-list 'same-window-buffer-names "*SQL*")


;; Style
(setq-default tab-width 4)

(require 'cc-mode)
(defconst erez-c-style
  '((c-basic-offset . 4)
    (c-offsets-alist . ((arglist-close . 0)
                        (substatement-open . 0)
                        (case-label . +)))))
(c-add-style "erez" erez-c-style)
(setq c-default-style "erez")

(require 'nxml-mode)
(setq nxml-child-indent 4)

;; Hooks
(add-hook 'text-mode-hook (lambda () (flyspell-mode)))
(add-hook 'c-mode-common-hook (lambda () (subword-mode t)))
(add-hook 'php-mode-hook (lambda () (flymake-mode t)))

(require 'whitespace)
(setq whitespace-style '(empty trailing))
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Turn off linum-mode for these modes
(add-hook 'sql-interactive-mode-hook (lambda () (linum-mode 0)))
(add-hook 'shell-mode-hook (lambda () (linum-mode 0)))

;; Load from external files
(add-to-list 'load-path "~/.emacs.d/")

;; Handle in external files
(require 'current-buffer)
(require 'region-rigidly)
(require 'smart-lines)
(require 'keys)
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

(require 'php-mode)
(setq php-mode-coding-style nil)
(setq php-mode-warn-if-mumamo-off nil)
