;;; ds4e-client.el --- Part of ds4e, interface to Docspell

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
;;; Some higher level functions executing dsc commands

;;; Code:


(require 'ds4e-dsc)
(require 'ds4e-helpers)

(defun ds4e-show-versions ()
  "Show the version of ds4e, dsc and the docspell server."
  (interactive)
  (let* ((result (ds4e-dsc-sync "version" nil))
         (cversion (ds4e-result-get result :client :build_version))
         (cgit (ds4e-result-get result :client :git_commit))
         (cdate (ds4e-result-get result :client :build_date))
         (url (ds4e-result-get result :docspell_url))
         (sversion (ds4e-result-get result :server :version))
         (sgit (ds4e-result-get result :server :git_commit))
         (sdate (ds4e-result-get result :server :built_at_string)))
    (message "Url: %s\nDs4e: %s\nDsc: %s (#%s, %s)\nServer %s (#%s, %s)"
             url
             ds4e-version
             cversion
             (substring cgit 0 7)
             (substring cdate 0 19)
             sversion
             (substring sgit 0 7)
             (substring sdate 0 19))))

(defun ds4e-login (&optional user pass)
  "Login to docspell server, optionally specifying USER and PASS."
  (interactive)
  (let* ((result
          (ds4e-dsc-sync "login" (list :user user :password pass)))
         (coll (ds4e-result-get result :collective))
         (user (ds4e-result-get result :user))
         (account (if (string-equal coll user)
                      user
                    (format "%s/%s" coll user)))
         (msg (ds4e-result-get result :message)))
    (if (ds4e-result-get result :success)
        (message "%s: %s" msg account)
      (user-error "Error: %s" msg))))


(defun ds4e-client-upload (file &rest files)
  "Upload FILE and FILES.

The last argument may be a plist containing addititonal arguments
to use with the upload command."
  (let* ((settings
          (let ((last-el (car (last files))))
            (and last-el (eq (type-of last-el) 'cons) last-el)))
         (all-files
          (cons file
                (if settings
                    (nreverse (cdr (reverse files)))
                  files)))
         (args
          (append settings
                  (ds4e-mapcat (lambda (el)
                                 (list :arg el))
                               all-files)))
         (result
          (ds4e-dsc-sync "upload" args)))
    (message "debug: %s" args)
    (ds4e-basic-result result)))

(defun ds4e-client-open (file-or-id &optional print-only)
  "Open the given FILE-OR-ID in docspell web interface.

It runs 'dsc open-item …' which opens the default browser unless
PRINT-ONLY is t. The result is nil if no item exists, or a plist
with keys :url and :item-id containing corresponding values."
  (let* ((args (list :print-only (and print-only t)
                     :arg file-or-id))
         (result (ds4e-dsc-sync "open-item" args))
         (success (ds4e-result-get result :success))
         (url (ds4e-result-get result :url))
         (id (ds4e-result-get result :item_id)))
    (when (ds4e-result-get result :has_more)
      (message "More items available!"))
    (when (and success print-only)
      (message "Item available at %s" url))
    (unless success
      (message "No item found."))
    (and success
         (list :url url :item-id id))))

(defun ds4e-client-get-bookmarks ()
  "Return a list of bookmarks."
  (let* ((args (list :arg "get"))
         (result (ds4e-dsc-sync "bookmark" args)))
    (ds4e-result-get result :bookmarks)))

(defun ds4e-client-get-bookmark (name-or-id)
  "Return a bookmark given its NAME-OR-ID."
  (seq-find
   (lambda (bookmark)
     (let ((name (ds4e-result-get bookmark :name))
           (id (ds4e-result-get bookmark :id)))
       (or (string-equal name-or-id name)
           (string-equal name-or-id id))))
   (ds4e-client-get-bookmarks)))

(provide 'ds4e-client)
;;; ds4e-client.el ends here
