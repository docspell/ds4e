;;; ds4e-dashboard.el --- Part of ds4e, interface to Docspell

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
;;; This file contains functions to integrate with dashboard.el.

;;; Code:

(require 'ds4e-settings)
(require 'ds4e-client)
(require 'ds4e-item)
(require 'dashboard)

(defgroup ds4e-dashboard nil
  "Settings for adding docspell results to your Emacs dashboard."
  :group 'ds4e
  :group 'dashboard)

(defcustom ds4e-dashboard-action-function
  nil
  "The function to invoke with an item structure when an item is selected."
  :group 'ds4e-dashboard
  :type 'function)


(defcustom ds4e-dashboard-item-format
  '(:correspondent " - " :name)
  "A list of either keywords or strings defining how to present the item.
A keyword must be from `ds4e-item-field-info' or
`ds4e-item-field-custom', strings are passed as given. All values
will finally be joined."
  :group 'ds4e-dashboard
  :type 'list)

(defcustom ds4e-dashboard-section-name "Docspell"
  "The section name to use."
  :type 'string
  :group 'ds4e-dashboard)

(defcustom ds4e-dashboard-identifier 'docspell
  "The symbol used to register the docspell section.

It is also used to lookup the shortcut. The shortcut to use needs
to be added to `dashboard-item-shortcuts'."
  :type 'symbol
  :group 'ds4e-dashboard)

(defcustom ds4e-dashboard-query '(:query "inbox:yes")
  "The query used to retrieve the list of items to display.

It is a plist that allows two keywords :query or :bookmark. If
:bookmark is present, the bookmark with that name is looked up.
Otherwise a :query is used as is."
  :type 'plist
  :group 'ds4e-dashboard)


(defun ds4e-dashboard-register ()
  "Registers the section in `dashboard-item-generators'."
  (add-to-list 'dashboard-item-generators
               (cons ds4e-dashboard-identifier 'ds4e-dashboard-list))
  (add-to-list 'dashboard-heading-icons
               (cons ds4e-dashboard-identifier "database")))

;;; Functions

(defun ds4e-dashboard--action (item)
  "Action when hitting an ITEM in the dashboard.
The default action is to open the item in the browser."
  (let ((id (ds4e-item-id item)))
    (ds4e-client-open id)))

(defun ds4e-dashboard--get-action ()
  "Return the action to use.

Returns a symbol holding the action function. The var
`ds4e-dashboard-action-function' or a default action is used."
  (or ds4e-dashboard-action-function
      'ds4e-dashboard--action))

(defun ds4e-dashboard--list-items (limit)
  "Return a flat list of items, at most LIMIT length."
  (let* ((query
          (let* ((bm (plist-get ds4e-dashboard-query :bookmark))
                 (qs (or (plist-get ds4e-dashboard-query :query) ""))
                 (bq (and bm
                          (ds4e-result-get (ds4e-client-get-bookmark bm) :query))))
            (or bq qs)))
         (args (list :limit limit
                     :offset 0
                     :arg query))
         (result (ds4e-dsc-sync "search" args))
         (groups (ds4e-result-get result :groups)))
    (apply 'append
           (mapcar (lambda (group)
                     (ds4e-result-get group :items))
                   groups))))

(defun ds4e-dashboard-list (list-size)
  "Define the dashboard section inserting LIST-SIZE items.

What items to insert is defined by a query `ds4e-dashboard-query'."
  (dashboard-insert-section
   ds4e-dashboard-section-name
   (condition-case nil
       (ds4e-dashboard--list-items list-size)
     (error nil
            (list "--- Dsc failed. Login? ---")))
   list-size
   'docspell
   (dashboard-get-shortcut ds4e-dashboard-identifier)
   `(lambda (&rest _)
      (and (listp (quote ,el))
           (funcall (ds4e-dashboard--get-action) (quote ,el))))
   (cond ((stringp el) el)
         (t (ds4e-item-format ds4e-dashboard-item-format el)))))


(provide 'ds4e-dashboard)
;;; ds4e-dashboard.el ends here
