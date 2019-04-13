;; init.el --- Emacs initialization file -*- lexical-binding: t -*-

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
(add-to-list 'load-path "~/.emacs.d/lisp")

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

;; Keep customize from modifying this file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; Default frame.
(add-to-list 'default-frame-alist '(font . "Inconsolata Medium 14"))

(setq kill-do-not-save-duplicates t)
(setq mode-require-final-newline t)
(setq next-line-add-newlines nil)
(setq save-abbrevs 'silently)
(setq sentence-end-double-space nil)
(setq-default fill-column 79)
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)
(setq-default tab-width 8)
(setq-default truncate-lines t)

(require 'grep)
(setq grep-find-use-xargs 'exec)

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

;; Fix ibuffer to use ido-find-file
(require 'ibuffer)
(define-key ibuffer-mode-map (kbd "C-x C-f") #'ido-find-file)
;; Always use ibuffer
(global-set-key [remap list-buffers] #'ibuffer)

(require 'nxml-mode)
(setq nxml-child-indent 4)

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

;; Python
(require 'python)
(setq python-shell-interpreter "python3")
(global-set-key (kbd "<f9>") #'run-python)

;; SQL
(require 'sql)

(defun project-config ()
  "Read and return JSON project config."
  (json-read-file (projectile-expand-root "config.json")))

(defun database (key)
  "Return the name of the database for the current project."
  (ignore-errors
    (let* ((config (project-config))
           (database-config (cdr (assoc 'database config)))
           (database (cdr (assoc key database-config))))
      database)))

(defun project-sql (product)
  "Run PRODUCT database with default database for current project."
  (let ((default-directory (expand-file-name "~"))
        (sql-database (database 'name))
        (sql-user (database 'user)))
    (sql-product-interactive product)))

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
  '("*compilation*"))

(defun kill-all-buffers ()
  "Kill all buffers except global buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (and (string-match "^\\*.*\\*$" (buffer-name buffer))
                 (not (member (buffer-name buffer) kill-all-global-buffers)))
      (kill-buffer buffer)))
  (grep-a-lot-clear-stack))

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

(use-package atomic-chrome
  :config (atomic-chrome-start-server))

(use-package crontab-mode
  :mode (("\\.cron\\(tab\\)?\\'" . crontab-mode)
         ("cron\\(tab\\)?\\." . crontab-mode)))

(use-package diff-hl
  :config (global-diff-hl-mode 1))

(use-package flx-ido
  :init (setq ido-auto-merge-work-directories-length -1
              ido-create-new-buffer 'never
              ido-enable-flex-matching t
              ido-enable-last-directory-history t
              ido-use-faces nil)
  :config (progn
            (ido-mode 1)
            (ido-everywhere 1)
            (flx-ido-mode 1)))

(use-package flycheck
  :init (progn
          (setq flycheck-highlighting-mode 'lines
                flycheck-display-errors-function nil)
          (setq-default flycheck-javascript-jshint-executable (expand-file-name "~/node_modules/.bin/jshint")))
  :config (global-flycheck-mode 1))

(defun init-git-commit-mode ()
  "Initialize GIT-COMMIT-MODE."
  (setq fill-column 72))

(use-package git-commit
  :config (progn
            (global-git-commit-mode 1)
            (add-hook 'git-commit-mode-hook #'init-git-commit-mode)))

(use-package grep-a-lot
  :config (grep-a-lot-setup-keys))

(use-package groovy-mode)

(use-package less-css-mode)

(use-package magit)

(use-package markdown-mode)

(use-package mwim
  :config (global-set-key [remap move-beginning-of-line] #'mwim-beginning-of-code-or-line))

(use-package nginx-mode)

(defun init-php-mode ()
  "Initialize PHP-MODE."
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-arglist)
  (setq tab-width 8))

(use-package php-mode
  :init (setq php-template-compatibility nil
              php-mode-warn-if-mumamo-off nil
              php-mode-coding-style 'psr2)
  :config (add-hook 'php-mode-psr2-hook #'init-php-mode))

(use-package pip-requirements)

(use-package pony-mode)

(use-package projectile
  :config (progn
            (add-to-list 'projectile-globally-ignored-directories ".mypy_cache")
            (add-to-list 'projectile-globally-ignored-directories ".serverless")
            (add-to-list 'projectile-globally-ignored-directories "__pycache__")
            (add-to-list 'projectile-globally-ignored-directories "__snapshots__")
            (add-to-list 'projectile-globally-ignored-directories "_build")
            (add-to-list 'projectile-globally-ignored-directories "bower_components")
            (add-to-list 'projectile-globally-ignored-directories "build")
            (add-to-list 'projectile-globally-ignored-directories "dist")
            (add-to-list 'projectile-globally-ignored-directories "node_modules")
            (add-to-list 'projectile-globally-ignored-directories "vendor")
            (add-to-list 'projectile-globally-ignored-directories "venv")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".d")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".gz")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".map")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".min.css")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".min.js")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".png")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".svg")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".woff")
            (add-to-list 'projectile-globally-ignored-files ".coverage")
            (add-to-list 'projectile-globally-ignored-files ".php_cs.cache")
            (add-to-list 'projectile-globally-ignored-files ".phpunit.result.cache")
            (add-to-list 'projectile-globally-ignored-files "ansible.log")
            (add-to-list 'projectile-globally-ignored-files "composer.lock")
            (add-to-list 'projectile-globally-ignored-files "npm-shrinkwrap.json")
            (add-to-list 'projectile-globally-ignored-files "taskconf.php")
            (add-to-list 'projectile-globally-ignored-files "urlconf.php")
            (projectile-mode 1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(use-package rjsx-mode
  :mode "\\.js\\'")

(use-package s)

(use-package systemd)

(use-package undo-tree
  :config (global-undo-tree-mode 1))

(use-package yaml-mode)

;; Additional extensions.
(require 'myproject)
