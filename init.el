;;; init.el --- Emacs initialization file

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
(setq inhibit-splash-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq default-frame-alist '((font . "Inconsolata-12")))
(set-scroll-bar-mode 'right)
(prefer-coding-system 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p)
(let ((backup-directory "~/.local/share/emacs"))
  (make-directory backup-directory t)
  (add-to-list 'backup-directory-alist `("^.*$" . ,backup-directory)))
(setq-default truncate-lines t)
(setq next-line-add-newlines nil)
(setq-default require-final-newline t)
(blink-cursor-mode t)
;(setq-default indent-tabs-mode nil)
(setq next-screen-context-lines 4)
(setq sentence-end-double-space nil)
(setq grep-find-use-xargs 'exec)

;; Enable ido mode
(require 'ido)
(ido-mode 1)

;; Fix copy-paste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Enable functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Global minor modes
(setq column-number-mode t)
(show-paren-mode t)
(delete-selection-mode t)
(add-hook 'find-file-hook (lambda () (subword-mode t)))
(add-hook 'text-mode-hook (lambda () (flyspell-mode)))

;; Auto revert mode
(require 'autorevert)
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Highlight line mode
(global-hl-line-mode t)
(set-face-background 'hl-line "light cyan")

;; Line number mode
(global-linum-mode t)
;; Except these modes
(add-hook 'sql-interactive-mode-hook (lambda () (linum-mode 0)))
(add-hook 'shell-mode-hook (lambda () (linum-mode 0)))

;; Save place mode
(require 'saveplace)
(setq-default save-place t)
(let ((save-place-directory "~/.cache/emacs"))
  (make-directory save-place-directory t)
  (setq save-place-file (concat save-place-directory "/saved-places")))

;; Whitespace mode
(require 'whitespace)
(setq whitespace-style '(empty trailing))
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Fix ibuffer to use ido-find-file
(require 'ibuffer)
(define-key ibuffer-mode-map (kbd "C-x C-f") 'ido-find-file)

;; Show in the current window
(add-to-list 'same-window-buffer-names "*grep*")
(add-to-list 'same-window-regexps "\\*grep\\*<[[:digit:]]+>")
(add-to-list 'same-window-buffer-names "*SQL*")

(defadvice split-window-right (after rebalance-windows activate)
  "Balance windows after splitting horizontally."
  (balance-windows))

(defadvice split-window-below (after rebalance-windows activate)
  "Balance windows after splitting vertically."
  (balance-windows))

;; Style
(setq-default tab-width 4)

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
(global-set-key (kbd "<backtab>") (lambda () (interactive) (insert "\t")))
;; Auto-indent
(global-set-key (kbd "RET") 'newline-and-indent)
;; Always kill the current buffer without asking
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer)))
;; Collapse lines
(global-set-key (kbd "M-j") (lambda () (interactive (join-line -1))))

;; Keys to enter common modes
(global-set-key (kbd "<f11>") 'shell)
(global-set-key (kbd "<f12>") 'sql-mysql)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace or beginning of the line."
  (interactive)
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      (beginning-of-line))))

(defun smart-end-of-line ()
  "Move point to last non-whitespace or end of the line."
  (interactive)
  (let ((pos (point)))
    (end-of-line)
    (re-search-backward "[^ \t]" (line-end-position 0) t)
    (forward-char)
    (when (= pos (point))
      (end-of-line))))

(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)
(global-set-key (kbd "<end>") 'smart-end-of-line)
(global-set-key (kbd "C-e") 'smart-end-of-line)

(defun rename-current-buffer-file (new-file-name)
  "Rename visited file of current buffer to NEW-FILE-NAME."
  (interactive "FNew name: ")
  (let ((name (buffer-name))
        (new-name (file-name-nondirectory new-file-name)))
    (if (not (and buffer-file-name (file-exists-p buffer-file-name)))
        (error "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (error "A buffer named '%s' already exists!" new-name)
        (rename-file buffer-file-name new-file-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-file-name)
        (set-buffer-modified-p nil)
        (message "File '%s' successfully renamed to '%s'" name new-name)))))

(defun delete-current-buffer-file ()
  "Delete visted file of current buffer."
  (interactive)
  (let ((file-name buffer-file-name))
    (if (not (and file-name (file-exists-p file-name)))
        (kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (kill-buffer)
        (delete-file file-name)
        (message "File '%s' successfully removed" file-name)))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

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

;; Easily open files as root
(require 'tramp)

(defvar file-name-root-history nil
  "History list of file names entered in the minibuffer as root.")

(defun find-file-root ()
  "Open a file as the root user."
  (interactive)
  (let* ((file-name-history file-name-root-history)
         (file-name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p file-name)
                     (tramp-dissect-file-name file-name)))
         (default (if tramp (tramp-file-name-localname tramp) file-name))
         (directory (file-name-directory default))
         (file-name (read-file-name "Find file [root]: " directory default)))
    (when file-name
      (find-file (concat "/sudo:root@localhost:" (expand-file-name file-name)))
      (setq file-name-root-history file-name-history))))

(global-set-key (kbd "C-S-x C-S-f") 'find-file-root)

;; A more convenient grep
(defun rgrep-project (regexp)
  "Recursively grep for REGEXP in the project root directory."
  (interactive "sSearch for: ")
  (grep-compute-defaults)
  (rgrep regexp "*" (if vc-mode
                        (vc-call root default-directory)
                      default-directory)))

(global-set-key (kbd "C-c C-g") 'rgrep-project)

;; libs
(eval-and-compile
  (require 'package)

  (defun require-packages (&rest packages)
    (mapc (lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
          packages))

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
                    'undo-tree
                    'vlf))

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(require 'flycheck)
(setq flycheck-highlighting-mode 'lines)
(set-face-background 'flycheck-error-face "light pink")
(set-face-background 'flycheck-warning-face "light goldenrod")

(add-hook 'find-file-hook
          (lambda ()
            (unless (or (tramp-tramp-file-p buffer-file-name)
                        (> (buffer-size) large-file-warning-threshold))
              (flycheck-mode))))

(require 'grep-a-lot)
(grep-a-lot-setup-keys)

(require 'php-mode)
(setq php-mode-coding-style nil)
(setq php-mode-warn-if-mumamo-off nil)

(require 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'javascript 'nxml)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'vlf)
(add-hook 'find-file-hook
          (lambda ()
            (when (> (buffer-size) large-file-warning-threshold)
              (vlf-mode)
              (buffer-disable-undo)
              (linum-mode 0)
              (undo-tree-mode 0))))

;;; init.el ends here
