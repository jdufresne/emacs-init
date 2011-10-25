;;; sudo.el ---

;; Copyright (C) 2011  Jon

;; Author: Jon <jon@jon-workstation.erezlife>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
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

;;


;;; Code:
(defun sudo-find-file (file-name)
  (interactive "FSudo Find File: ")
  (find-file (concat "/sudo::" (expand-file-name file-name))))

(global-set-key (kbd "C-x C-r") 'sudo-find-file)


(provide 'sudo)
;;; sudo.el ends here
