;;; init.el --- Emacs initialization file -*- lexical-binding: t -*-

;; Author: Jon Dufresne <jon@jondufresne.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Initialize Emacs the way I like it.

;;; Code:

;; Basic config

(menu-bar-mode 0)
(if (functionp 'tool-bar-mode)
    (tool-bar-mode 0))
(scroll-bar-mode 0)
(setq create-lockfiles nil)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

(let ((local-bin (expand-file-name "~/.local/bin")))
  (unless (member local-bin exec-path)
    (add-to-list 'exec-path local-bin)
    (setenv "PATH" (concat local-bin path-separator (getenv "PATH")))))

;; Keep customize from modifying this file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; Default frame.
(add-to-list 'default-frame-alist '(font . "Fira Mono 14"))

(setq kill-do-not-save-duplicates t)
(setq mode-require-final-newline t)
(setq next-line-add-newlines nil)
(setq save-abbrevs 'silently)
(setq sentence-end-double-space nil)
(setq-default c-basic-offset 4)
(setq-default fill-column 79)
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)
(setq-default tab-width 8)
(setq-default truncate-lines t)

(require 'sort)
(setq sort-fold-case t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Enable functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Global minor modes
(setq column-number-mode t)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-display-line-numbers-mode)
(global-subword-mode 1)
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook #'turn-on-flyspell)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Auto revert mode
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(global-auto-revert-mode 1)

;; Save place mode
(require 'saveplace)
(save-place-mode t)
(savehist-mode 1)

;; Buffer clean up
(prefer-coding-system 'utf-8)
(require 'whitespace)
(defun cleanup-buffer ()
  "Set the preferred style upon save."
  (set-buffer-file-coding-system 'utf-8)
  (let ((whitespace-style '(empty trailing)))
    (whitespace-cleanup)))
(add-hook 'before-save-hook #'cleanup-buffer)

;; Always use ibuffer
(global-set-key [remap list-buffers] #'ibuffer)

;; Remove annoying keys
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Always kill the current buffer without asking
(defun kill-buffer-now (&optional buffer-or-name)
  "Kill the buffer specified by BUFFER-OR-NAME without asking."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer buffer-or-name)))
(global-set-key (kbd "C-x k") #'kill-buffer-now)
(global-set-key (kbd "C-x C-k") #'kill-buffer-now)

(defun copy-file-name-as-kill ()
  "Copy the current buffer file name to the kill ring."
  (interactive)
  (let ((file-name (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when file-name
      (kill-new file-name)
      (message "Copied buffer file name '%s' to the clipboard." file-name))))

;; Python
(require 'python)
(setq python-fill-docstring-style 'django)
(global-set-key (kbd "<f9>") #'run-python)

;; SQL
(require 'sql)

(defun project-sql (product)
  "Run PRODUCT database with default database for current project."
  (let ((default-directory (expand-file-name "~")))
    (sql-product-interactive product))
  (pop-to-buffer (sql-find-sqli-buffer product)))

(setq sql-postgres-login-params '((database :default "literacyfootprints")))

(defun project-sql-postgres ()
  "Run PostgreSQL with default database for current project."
  (interactive)
  (project-sql 'postgres))

(global-set-key (kbd "<f12>") #'project-sql-postgres)

(defun init-sql-mode ()
  "Initialize SQL-MODE."
  (setq sql-buffer (get-buffer "*SQL*")))
(add-hook 'sql-mode-hook #'init-sql-mode)

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

(global-set-key (kbd "M-Q") #'unfill-paragraph)
(global-set-key (kbd "C-M-Q") #'unfill-region)

(defun insert-file-name ()
  "Insert the buffer's file name sans final extension at point."
  (interactive)
  (when (buffer-file-name)
    (insert (file-name-base (buffer-file-name)))))

;; Speed up large files such as SQL backups
(defun init-large-buffer ()
  "Setup large buffers to better handle large buffers."
  (when (> (buffer-size) large-file-warning-threshold)
    (setq buffer-read-only t)
    (buffer-disable-undo)))
(add-hook 'find-file-hook #'init-large-buffer)

(defvar kill-all-global-buffers
  '("^\\*compilation\\*$"))

(defun kill-all-buffers ()
  "Kill all buffers except global buffers."
  (interactive)
  (call-interactively #'deadgrep-kill-all-buffers)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (unless (and (string-match "^\\*.*\\*$" name)
                   (not (some (lambda (regex) (string-match regex name))
                              kill-all-global-buffers)))
        (kill-buffer buffer)))))

(require 'rst)
(setq rst-indent-literal-minimized 4)
(add-to-list 'auto-mode-alist '("/docs/.*\\.txt\\'" . rst-mode))

(load "~/.emacs.d/env" t)

;; Third party libraries.

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t)

(use-package company
  :init (add-hook 'after-init-hook #'global-company-mode))

(use-package crontab-mode
  :mode "\\(?:\\.\\|/\\)\\(?:cron\\(?:tab\\)?\\)\\'")

(use-package deadgrep)

(use-package dockerfile-mode)

(use-package doom-themes
  :init (setq doom-themes-enable-bold t
	      doom-themes-enable-italic t)
  :config (progn
	    (load-theme 'doom-one t)
	    (doom-themes-visual-bell-config)))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package enh-ruby-mode
  :mode "\\(?:\\.\\(?:axlsx\\|rbw?\\|ru\\|rake\\|thor\\|jbuilder\\|rabl\\|gemspec\\|podspec\\)\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Puppet\\|Berks\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'")

(use-package flycheck
  :init (progn
	  (setq flycheck-highlighting-mode 'lines
		flycheck-display-errors-function nil)
	  (setq-default flycheck-disabled-checkers '(python-mypy)))
  :config (global-flycheck-mode 1))

(use-package groovy-mode)

(use-package helm
  :init (setq-default helm-M-x-fuzzy-match t)
  :config (progn
	    (global-set-key [remap bookmark-jump] #'helm-filtered-bookmarks)
	    (global-set-key [remap execute-extended-command] #'helm-M-x)
	    (global-set-key [remap find-file] #'helm-find-files)
	    (helm-mode 1)))

(use-package jenkinsfile-mode)

(use-package magit
  :hook (git-commit-setup . (lambda () (setq fill-column 72))))

(use-package markdown-mode)

(use-package mwim
  :config (global-set-key [remap move-beginning-of-line] #'mwim-beginning-of-code-or-line))

(use-package nginx-mode)

(use-package pip-requirements)

(use-package project)

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode 1))

(use-package rjsx-mode
  :mode "\\.jsx?\\'"
  :bind (:map rjsx-mode-map
              ("<" . nil)
              ("C-d" . nil)
              (">" . nil)))

(use-package rspec-mode
  :config (setq rspec-use-rvm t))

(use-package rvm
  :config (rvm-use-default))

(use-package systemd)

(use-package sql-indent)

(use-package terraform-mode)

(use-package undo-tree
  :init (setq-default undo-tree-auto-save-history nil)
  :config (global-undo-tree-mode 1))

(use-package web-mode
  :config (setq web-mode-enable-auto-indentation nil
                web-mode-enable-auto-closing nil
                web-mode-enable-auto-pairing nil
                web-mode-enable-auto-opening nil
                web-mode-enable-auto-quoting nil)
  :mode "\\.html.erb\\'")

(use-package yaml-mode)

;; Project convenience fucntions

(require 'projectile)

(global-set-key [remap projectile-grep] #'deadgrep)

(defconst rails-buffer-name "*rails*")
(defconst webpack-buffer-name "*webpack*")
(defconst routes-buffer-name "*routes*")

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Colorize compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun goto-buffer-end-in-windows (buffer-name)
  "Set point to end in BUFFER-NAME."
  (with-current-buffer buffer-name
    (dolist (window (get-buffer-window-list))
      (set-window-point window (point-max)))))

(defun buffer-name-function (buffer-name)
  "Make a function that return BUFFER-NAME."
  (lambda (_name-of-mode) buffer-name))

(defun kill-buffer-if-exists (buffer-name)
  "Kill the buffer specified by BUFFER-NAME."
  (let ((kill-buffer-query-functions nil)
        (buffer (get-buffer buffer-name)))
    (when buffer
      (kill-buffer buffer))))

(defun kill-server ()
  "Kill development server buffers."
  (interactive)
  (kill-buffer-if-exists rails-buffer-name)
  (kill-buffer-if-exists webpack-buffer-name))

(defun compile-to-buffer (buffer-name command)
  "Run the COMMAND and send output to BUFFER-NAME."
  (let ((compilation-buffer-name-function (buffer-name-function buffer-name))
        (compilation-save-buffers-predicate #'ignore))
    (compile command))
  (goto-buffer-end-in-windows buffer-name))

(defun bundle-exec-command (command)
  "Format bundler exec COMMAND with the project's Ruby version."
  (format "bundle exec %s" command))

(defun project-run-server ()
  "Run the development server."
  (interactive)
  (kill-server)
  (let ((default-directory (projectile-acquire-root)))
    (compile-to-buffer webpack-buffer-name "npx webpack serve")
    (compile-to-buffer rails-buffer-name (bundle-exec-command "rails server"))))

(defun project-routes ()
  "Test the project."
  (interactive)
  (let ((default-directory (projectile-acquire-root))
        (compilation-buffer-name-function (buffer-name-function routes-buffer-name)))
    (compile (bundle-exec-command "rails routes")))
  (goto-buffer-end-in-windows routes-buffer-name)
  (pop-to-buffer routes-buffer-name))

(global-set-key (kbd "S-<f5>") #'kill-server)
(global-set-key (kbd "<f5>") #'project-run-server)
(global-set-key (kbd "<f7>") #'project-routes)

(provide 'init)

;;; init.el ends here
