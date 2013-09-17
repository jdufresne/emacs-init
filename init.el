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
(setq inhibit-splash-screen t)

(setq default-frame-alist '((auto-raise . t)
                            (font . "Inconsolata Medium 12")))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default truncate-lines t)
(setq next-line-add-newlines nil)
(setq-default require-final-newline t)
(setq sentence-end-double-space nil)
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
(add-hook 'after-make-frame-functions 'init-frame)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(eval-and-compile (add-to-list 'load-path "~/.emacs.d/"))
(require 'project)

;; Enable ido mode
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-create-new-buffer 'never)
(setq ido-enable-flex-matching t)
(setq ido-enable-last-directory-history t)
(setq ido-use-virtual-buffers t)

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
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Auto revert mode
(require 'autorevert)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Save place mode
(require 'saveplace)
(setq-default save-place t)

;; Whitespace mode
(require 'whitespace)
(prefer-coding-system 'utf-8)

(defun cleanup-buffer ()
  "Set the preferred style upon save."
  (set-buffer-file-coding-system buffer-file-coding-system)
  (let ((whitespace-style '(empty trailing)))
    (whitespace-cleanup)))
(add-hook 'before-save-hook 'cleanup-buffer)

;; Fix ibuffer to use ido-find-file
(require 'ibuffer)
(define-key ibuffer-mode-map (kbd "C-x C-f") 'ido-find-file)
;; Always use ibuffer
(global-set-key [remap list-buffers] 'ibuffer)

;; Show in the current window
(add-to-list 'same-window-regexps "\\*grep\\*\\(?:<[[:digit:]]+>\\)?")
(add-to-list 'same-window-buffer-names "*SQL*")

;; Style
(require 'cc-mode)
(defconst erez-c-style '((c-basic-offset . 4)
                         (c-offsets-alist . ((arglist-close . 0)
                                             (substatement-open . 0)
                                             (case-label . +)))))
(c-add-style "erez" erez-c-style)
(setq c-default-style "erez")

(require 'nxml-mode)
(setq nxml-child-indent 4)


;; Remove annoying keys
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Insert a tab
(defun indent-tab-rigidly (start end)
  "Indent lines from START to END rigidly by `tab-width'."
  (interactive "r")
  (unless (region-active-p)
    (setq start (line-beginning-position)
          end (line-end-position)))
  (let (deactivate-mark)
    (indent-rigidly start end tab-width)))

(global-set-key (kbd "<backtab>") 'indent-tab-rigidly)

;; Auto-indent
(global-set-key (kbd "RET") 'newline-and-indent)
;; Always kill the current buffer without asking
(defun kill-buffer-now (&optional buffer-or-name)
  "Kill the buffer specified by BUFFER-OR-NAME without asking."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer buffer-or-name)))
(global-set-key (kbd "C-x k") 'kill-buffer-now)
(global-set-key (kbd "C-x C-k") 'kill-buffer-now)

;; Keys to enter common modes
(global-set-key (kbd "<f12>") 'sql-mysql)

(defun init-sql-mode ()
  "Initialize SQL-MODE.

Turn off LINUM-MODE, as the buffer can be extremely large.  Change
directory to home."
  (linum-mode 0)
  (cd (expand-file-name "~/")))
(add-hook 'sql-interactive-mode-hook 'init-sql-mode)

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

(global-set-key [remap move-beginning-of-line] 'smart-move-beginning-of-line)
(global-set-key [remap move-end-of-line] 'smart-move-end-of-line)

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

;; Speed up large files such as SQL backups
(defun init-large-buffer ()
  "Setup large buffers to better handle large buffers."
  (when (> (buffer-size) large-file-warning-threshold)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (linum-mode 0)))
(add-hook 'find-file-hook 'init-large-buffer)

(defun kill-all-buffers ()
  "Kill all buffers except global buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (string-match "^\\*.*\\*$" (buffer-name buffer))
      (kill-buffer buffer))))


;; Third party libraries.
(require 'package)
(defun require-packages (packages)
  "Install each package in PACKAGES unless already installed."
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(eval-and-compile
  (package-initialize))

(package-refresh-contents)
(require-packages '(apache-mode
                    browse-kill-ring
                    fill-column-indicator
                    flycheck
                    grep-a-lot
                    php-mode
                    rainbow-mode
                    smart-tabs-mode
                    undo-tree
                    web-mode))

;; Initialize third party libraries.

(require 'apache-mode)
(add-to-list 'auto-mode-alist '("\\.conf$" . apache-mode))

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(defun fci-mode-on ()
  "Turn fci-mode on."
  (fci-mode 1))
(define-globalized-minor-mode global-fci-mode
  fci-mode
  fci-mode-on)
(global-fci-mode 1)

(require 'flycheck)
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-display-errors-function nil)
(global-flycheck-mode 1)

(require 'grep-a-lot)
(grep-a-lot-setup-keys)

(require 'php-mode)
(setq php-mode-coding-style nil)
(setq php-mode-warn-if-mumamo-off nil)

(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-turn-on)

(require 'smart-tabs-mode)
(defun guess-tabs-mode ()
  "Guess tabs style of current buffer."
  (when (> (how-many "^\t" (point-min) (point-max))
           (how-many "^  " (point-min) (point-max)))
    (setq indent-tabs-mode t)
    (smart-tabs-mode 1)))
(add-hook 'prog-mode-hook 'guess-tabs-mode)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))

;;; init.el ends here
