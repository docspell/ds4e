;;; ds4e-direld.el --- Part of ds4e, interface to Docspell

;; Copyright © 2022- Eike Kettner

;; This file is not part of GNU Emacs.

;; ds4e is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ds4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ds4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; This provides some integration with Dired.

;;; Code:

(require 'ds4e-client)
(require 'dired)


;;;###autoload
(defun ds4e-dired-upload ()
  "Upload marked files or the file currently pointed."
  (interactive)
  (let* ((file-list (or (dired-get-marked-files)
                        (list (dired-get-filename))))
         (files (append file-list (and file-list '((:traverse t))))))
    (when files
      (apply 'ds4e-client-upload files))))

;;;###autoload
(defun ds4e-dired-open-browser ()
  "Open the current file in docspell, if existing there."
  (interactive)
  (let* ((file (dired-get-filename)))
    (and file
         (ds4e-client-open file))))


(provide 'ds4e-dired)
;;; ds4e-dired.el ends here
