;;; espeak.el --- Emacs espeak library -*- lexical-binding: t -*-

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

;; Elisp library to read text aloud using espeak-ng.

;;; Code:

(defun -espeak-string (string)
  "Start the espeak process with STRING."
  (make-process
   :name "espeak"
   :command `("espeak-ng" ,string)
   :noquery t))

(defun espeak-buffer ()
  "Read buffer aloud."
  (interactive)
  (-espeak-string (buffer-string)))

(defun espeak-region ()
  "Read region aloud."
  (interactive)
  (save-restriction
    (narrow-to-region (region-beginning) (region-end))
    (-espeak-string (buffer-string))))

(provide 'espeak)

;;; espeak.el ends here
