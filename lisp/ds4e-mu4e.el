;;; ds4e-mu4e.el --- Part of ds4e, interface to Docspell

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
;;; Defines two functions that can be used with mu4e's mail view to
;;; upload attachments or the complete mail to docspell.
;;;
;;; Code:

(require 'ds4e-client)
;(require 'mu4e-view)

(defun ds4e-mu4e--mime-upload (temp)
  "Expecting a TEMP file to upload."
  (let ((ds4e-dsc-display-error-buffer t))
    (ds4e-client-upload temp)))

(defun ds4e-mu4e--message-upload (msg)
  "Import the complete MSG to docspell."
  (let* ((subj (mu4e-message-field msg :subject))
         (path (mu4e-message-field msg :path))
         (subject (s-trim
                   (s-replace-regexp "[ \t\r\n]+" " "
                                     (s-replace-regexp "[:]" ""
                                                       (s-replace-all '( ("/" . "-")) subj)))))
         (temp (expand-file-name (concat "/tmp/" subject ".eml"))))
    ;; need to copy it, because there can be problems with maildir
    ;; filenames; it also is nicer, since the name is then the subject
    ;; and not some cryptic crap
    (copy-file path temp 1)
    (unwind-protect
        (let ((ds4e-dsc-display-error-buffer t))
          (ds4e-client-upload temp))
      (delete-file temp))))

(defun ds4e-mu4e-register (&optional label)
  "Add upload actions to the mu4e actions lists.

The optional argument LABEL can be used to customize the shortcut
labels."
  (let ((m-label (or label "Docspell")))
    (add-to-list 'mu4e-view-actions
                 `(,m-label . ds4e-mu4e--message-upload))
    (add-to-list 'mu4e-headers-actions
                 `(,m-label . ds4e-mu4e--message-upload))
    (add-to-list 'mu4e-view-mime-part-actions
                 (list :name m-label
                       :handler 'ds4e-mu4e--mime-upload
                       :receives 'temp))))


(provide 'ds4e-mu4e)
;;; ds4e-mu4e ends here
