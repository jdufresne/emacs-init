;;; init.el --- Emacs initialization file

;; Author: Jon Dufresne <jon@jondufresne.org>

;;; Commentary:

;; Initialize Emacs the way I like it.

;;; Code:

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

(require 'whitespace)
(setq whitespace-style '(empty trailing))
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Turn off linum-mode for these modes
(add-hook 'sql-interactive-mode-hook (lambda () (linum-mode 0)))
(add-hook 'shell-mode-hook (lambda () (linum-mode 0)))

;; Remove annoying keys
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Always kill the current buffer without asking
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer)))

(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-c C-g") 'grep)
(global-set-key (kbd "<f11>") 'shell)
(global-set-key (kbd "<f12>") 'sql-mysql)

(defun rename-current-buffer-file (new-file-name)
  "Rename file backed by current buffer to NEW-FILE-NAME."
  (interactive "FNew name: ")
  (let ((name (buffer-name))
        (file-name (buffer-file-name))
        (new-name (file-name-nondirectory new-file-name)))
    (if (not (and file-name (file-exists-p file-name)))
        (error "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (error "A buffer named '%s' already exists!" new-name)
        (rename-file file-name new-file-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-file-name)
        (set-buffer-modified-p nil)
        (message "File '%s' successfully renamed to '%s'" name new-name)))))

(defun delete-current-buffer-file ()
  "Delete file backed by current buffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (not (and file-name (file-exists-p file-name)))
        (kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (kill-buffer)
        (delete-file file-name)
        (message "File '%s' successfully removed" file-name)))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace or the beginning of the line."
  (interactive)
  (let ((pos (point)))
    (back-to-indentation)
    (if (= pos (point))
        (beginning-of-line))))

(defun smart-end-of-line ()
  "Move point to last non-whitespace or the end of the line."
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

(defun unfill-paragraph ()
  "Unfill paragraph at or after point."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  "Unfill each of the paragraphs in the region."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "C-M-Q") 'unfill-region)

;; libs
(eval-and-compile
  (require 'package)

  (defun require-packages (&rest packages)
    (dolist (package packages)
      (unless (package-installed-p package)
        (package-install package))))

  (package-initialize)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (unless (file-exists-p package-user-dir)
    (package-refresh-contents))

  (require-packages 'browse-kill-ring
                    'flycheck
                    'grep-a-lot
                    'php-mode
                    'smart-tabs-mode
                    'undo-tree))

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(require 'flycheck)
(add-hook 'find-file-hook 'flycheck-mode)

(require 'grep-a-lot)
(grep-a-lot-setup-keys)

(require 'php-mode)
(setq php-mode-coding-style nil)
(setq php-mode-warn-if-mumamo-off nil)

(require 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'javascript 'nxml)

(require 'undo-tree)
(global-undo-tree-mode)

(provide 'init)
;;; init.el ends here
