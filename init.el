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
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; Default frame alist
(add-to-list 'default-frame-alist '(auto-raise . t))
(add-to-list 'default-frame-alist '(font . "Inconsolata Medium 14"))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default truncate-lines t)
(setq next-line-add-newlines nil)
(setq-default require-final-newline t)
(setq mode-require-final-newline t)
(setq kill-do-not-save-duplicates t)
(setq grep-find-use-xargs 'exec)

(defun init-frame (frame)
  "Initialize a new FRAME.

Set the FRAME-TITLE-FORMAT to a useful format then raise the
frame."
  (setq frame-title-format
        '(:eval (if buffer-file-name
                    (abbreviate-file-name buffer-file-name)
                  "%b")))
  (select-frame-set-input-focus frame))
(add-hook 'after-make-frame-functions #'init-frame)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Default dictionary
(require 'ispell)
(setq ispell-dictionary "english")

;; Enable functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Global minor modes
(setq column-number-mode t)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-hl-line-mode 1)
(global-linum-mode 1)
(global-subword-mode 1)
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook #'turn-on-flyspell)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Auto revert mode
(require 'autorevert)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Save place mode
(require 'saveplace)
(setq-default save-place t)

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

;; A bit nicer.
(global-set-key [remap find-tag] #'find-tag-other-window)

;; SQL
(require 'sql)
(defun project-sql-mysql ()
  "Run MySQL with default database for current project."
  (interactive)
  (let ((sql-user "root")
        (sql-database (or (project-name) sql-database)))
    (call-interactively #'sql-mysql)))
(global-set-key (kbd "<f12>") #'project-sql-mysql)

(defun project-sql-postgres ()
  "Run PostgreSQL with default database for current project."
  (interactive)
  (let ((sql-database (or (project-name) sql-database)))
    (call-interactively #'sql-postgres)))
(global-set-key (kbd "<f9>") #'project-sql-postgres)


(defun init-sql-mode ()
  "Initialize SQL-MODE.

Turn off LINUM-MODE, as the buffer can be extremely large.  Change
directory to home."
  (linum-mode 0)
  (cd (expand-file-name "~/")))
(add-hook 'sql-interactive-mode-hook #'init-sql-mode)

(defun smart-move-beginning-of-line ()
  "Move point back to indentation or beginning of line."
  (interactive)
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (beginning-of-line))))

(defun smart-move-end-of-line ()
  "Move point to last non-whitespace or end of the line."
  (interactive)
  (let ((orig-point (point)))
    (end-of-line)
    (re-search-backward "[^[:space:]]" (line-end-position 0) t)
    (forward-char)
    (when (= orig-point (point))
      (end-of-line))))

(global-set-key [remap move-beginning-of-line] #'smart-move-beginning-of-line)
(global-set-key [remap move-end-of-line] #'smart-move-end-of-line)

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

;; Speed up large files such as SQL backups
(defun init-large-buffer ()
  "Setup large buffers to better handle large buffers."
  (when (> (buffer-size) large-file-warning-threshold)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (linum-mode 0)))
(add-hook 'find-file-hook #'init-large-buffer)

(defun kill-all-buffers ()
  "Kill all buffers except global buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (string-match "^\\*.*\\*$" (buffer-name buffer))
      (kill-buffer buffer)))
  (grep-a-lot-clear-stack))

(defun python-insert-encoding-comment ()
  "Insert a UTF-8 coding comment at the beginning of the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "# -*- coding: utf-8 -*-\n")
    (insert "from __future__ import unicode_literals\n")))

(defun python-insert-remote-debugger ()
  "Insert Python snippet to start the remote debugger."
  (interactive)
  (insert "import rpdb2; rpdb2.start_embedded_debugger('password')"))

(add-hook 'rst-mode-hook
          (lambda () (setq fill-column 79)))

;; Third party libraries.
(require 'package)
(defun require-packages (packages)
  "Install each package in PACKAGES unless already installed."
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(package-refresh-contents)
(require-packages '(apache-mode
                    company
                    crontab-mode
                    diff-hl
                    flx-ido
                    flycheck
                    grep-a-lot
                    less-css-mode
                    magit
                    php-mode
                    pony-mode
                    rainbow-mode
                    s
                    smart-tabs-mode
                    undo-tree))

;; Initialize third party libraries.
(require 'apache-mode)
(add-to-list 'auto-mode-alist '("\\.conf$" . apache-mode))

(require 'company)
(global-company-mode)

(require 'crontab-mode)
(add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\." . crontab-mode))

(require 'diff-hl)
(global-diff-hl-mode)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-create-new-buffer 'never)
(setq ido-enable-flex-matching t)
(setq ido-enable-last-directory-history t)
(setq ido-use-faces nil)

(require 'flycheck)
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-display-errors-function nil)
(setq-default flycheck-javascript-jshint-executable
              (expand-file-name "~/node_modules/.bin/jshint"))
(setq-default flycheck-javascript-eslint-executable
              (expand-file-name "~/node_modules/.bin/eslint"))
(setq-default flycheck-json-jsonlint-executable
              (expand-file-name "~/node_modules/.bin/jsonlint"))
(setq-default flycheck-disabled-checkers
              '(php-phpmd php-phpcs))
(global-flycheck-mode 1)

(require 'grep-a-lot)
(grep-a-lot-setup-keys)

(require 'php-mode)
(setq php-template-compatibility nil)
(setq php-mode-warn-if-mumamo-off nil)
(setq php-mode-coding-style 'psr2)
(add-hook 'php-mode-psr2-hook
          (lambda ()
            (c-set-offset 'arglist-cont-nonempty 'c-lineup-arglist)
            (setq tab-width 8)))

(require 'pony-mode)
(add-to-list 'pony-indenting-tags "span_icon")
(add-to-list 'pony-indenting-tags "a_icon")

(require 'rainbow-mode)
(add-hook 'css-mode-hook #'rainbow-turn-on)

(require 'smart-tabs-mode)
(defun guess-tabs-mode ()
  "Guess tabs style of current buffer."
  (when (> (how-many "^\t" (point-min) (point-max))
           (how-many "^  " (point-min) (point-max)))
    (setq indent-tabs-mode t)
    (setq tab-width 4)))
(add-hook 'prog-mode-hook #'guess-tabs-mode)
(add-hook 'text-mode-hook #'guess-tabs-mode)

(defun php-enable-smart-tabs-mode ()
  "Enable smart-tabs-mode for PHP files."
  (when indent-tabs-mode
    (smart-tabs-mode-enable)
    (smart-tabs-advice php-cautious-indent-line c-basic-offset)
    (smart-tabs-advice php-cautious-indent-region c-basic-offset)))
(add-hook 'php-mode-hook #'php-enable-smart-tabs-mode)

(defun js-enable-smart-tabs-mode ()
  "Enable smart-tabs-mode for JavaScript files."
  (when indent-tabs-mode
    (smart-tabs-mode-enable)
    (smart-tabs-advice js-indent-line js-indent-level)))
(add-hook 'js-mode-hook #'js-enable-smart-tabs-mode)

(require 'undo-tree)
(global-undo-tree-mode)


;; Additional extensions.

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/"))
(require 'project)
(require 'tidy)

;;; init.el ends here
