;;; ds4e-item.el --- part of ds4e, docspell interface

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
;;; Utility functions to retrieve data from an item.

;;; Code:

(require 'ds4e-helpers)

;; Available fields of an item

(defconst ds4e-item-field-info
  '((:id
     . (:name "Id"
        :help "The item id"))
    (:name
     . (:name "Name"
        :help "The item name"))
    (:correspondent
     . (:name "Correspondent"
        :help "Either corr. organization, person or both."))
    (:concerning
     . (:name "Concerinng"
        :help "Either concerning person, equipment, or both"))
    (:date
     . (:name "Date"
        :help "The item date."))
    (:due-date
     . (:name "Due Date"
        :help "The due date"))
    (:tags
     . (:name "Tags"
        :help "All tags of an item"))
    (:custom-fields
     . (:name "Custom Fields"
        :help "All custom fields of an item"))
    (:tags-and-fields
     . (:name "Tags/Fields"
        :help "Tags and fields of the item")))
  "All available fields of an item.
This is used in the search view to display columns.")

(defvar ds4e-item-field-custom
  '((:tags-doctype
     . (:name "Doctype"
              :help "The document type."
              :function (lambda (item &rest ignore)
                          (ds4e-item-tags-format (ds4e-item-tags item "doctype"))))))
  "A list of custom field definitions.
The format is similar to `ds4e-item-field-info', only with an
additional :function that returns a string given the item and two
optional arguments 'sep' and 'datefmt'. See `ds4e-item-get-field'.")


(defun ds4e-item-id (item)
  "Return the id of ITEM."
  (ds4e-result-get item :id))

(defun ds4e-item-name (item)
  "Return the name of the given ITEM."
  (ds4e-result-get item :name))

(defun ds4e-item-date (item &optional fmt)
  "Return the date of ITEM.
If FMT is present it is used to format the timestamp."
  (let ((ts (ds4e-result-get item :date)))
    (if (and fmt ts)
        (format-time-string "%Y-%m-%d" (/ ts 1000))
      ts)))

(defun ds4e-item-due-date (item &optional fmt)
  "Return the due-date of ITEM.
If FMT is present it is used to format the timestamp."
  (let ((ts (car (ds4e-result-get item :due_date))))
    (if (and fmt ts)
        (format-time-string "%Y-%m-%d" (/ ts 1000))
      ts)))

(defun ds4e-item-due-p (item)
  "Return whether ITEM has a due date set."
  (and (ds4e-result-get item :due_date) t))

(defun ds4e-item-state (item)
  "Return the state of ITEM."
  (ds4e-result-get item :state))

(defun ds4e-item-confirmed-p (item)
  "Return whether ITEM is in confirmed state."
  (string-equal (ds4e-item-state item) "confirmed"))

(defun ds4e-item-created-p (item)
  "Return whether ITEM is in created state."
  (string-equal (ds4e-item-state item) "created"))

(defun ds4e-item-corr-org (item)
  "Return corr. organization of ITEM."
  (ds4e-result-get item :corr_org :name))

(defun ds4e-item-corr-person (item)
  "Return corr. person of ITEM."
  (ds4e-result-get item :corr_person :name))

(defun ds4e-item-correspondent (item &optional sep)
  "Return corr. organization, person or both of ITEM.
If both are present, they are separated by SEP."
  (let ((org (ds4e-item-corr-org item))
        (pers (ds4e-item-corr-person item)))
    (cond ((and org pers) (concat  org (or sep "/") pers))
          (org org)
          (t pers))))

(defun ds4e-item-conc-person (item)
  "Return the concerning person name of ITEM."
  (ds4e-result-get item :conc_person :name))

(defun ds4e-item-conc-equip (item)
  "Return the concerning equipment name of ITEM."
  (ds4e-result-get item :conc_equip :name))

(defun ds4e-item-concerning (item &optional sep)
  "Return concering person, equip or both of ITEM separated by SEP."
  (let ((pers (ds4e-item-conc-person item))
        (equip (ds4e-item-conc-equip item)))
    (cond ((and pers equip) (concat pers (or sep "/") equip))
          (pers pers)
          (t equip))))

(defun ds4e-item-tags (item &optional category)
  "Return a list of tag names of ITEM.
If CATEGORY is a string, it is used to filter all tags of that
catgory."
  (let ((tags (ds4e-result-get item :tags)))
    (mapcar (lambda (tag)
              (ds4e-result-get tag :name))
            (if category
                (seq-filter (lambda (tag)
                              (string-equal (car (ds4e-result-get tag :category)) category))
                            tags)
              tags))))

(defun ds4e-item-custom-fields (item)
  "Return a list of custom field strings of ITEM.
An element consists of 'label:value'."
  (mapcar (lambda (cf)
            (let ((name (ds4e-result-get cf :name))
                  (label (ds4e-result-get cf :label))
                  (value (ds4e-result-get cf :value)))
              (concat (or (car label) name) ":" value)))
          (ds4e-result-get item :customfields)))

(defun ds4e-item-tags-format (tags &optional sep beg end)
  "Return a single string of TAGS names.
Tags are separated by SEP and the string is prefixed by BEG and
suffixed by END."
  (let ((tags-joined (string-join tags (or sep ", "))))
    (unless (string-equal "" tags-joined)
      (concat (or beg "") tags-joined (or end "")))))

(defun ds4e-item-get-field (item field &optional datefmt sep)
  "Return value of FIELD in ITEM.
FIELD is a keyword from `ds4e-item-field-info' or
`ds4e-item-field-custom'. DATEFMT is used to format date values
and SEP separates correspondent and concerning values in case
both are available."
  (let ((custom (plist-get
                 (alist-get field ds4e-item-field-custom)
                 :function)))
    (cond (custom (funcall custom item datefmt sep))
          ((eq field :date) (ds4e-item-date item datefmt))
          ((eq field :id) (ds4e-item-id item))
          ((eq field :name) (ds4e-item-name item))
          ((eq field :correspondent) (ds4e-item-correspondent item sep))
          ((eq field :concerning) (ds4e-item-concerning item sep))
          ((eq field :due-date) (ds4e-item-due-date item datefmt))
          ((eq field :tags) (ds4e-item-tags-format (ds4e-item-tags item) ", "))
          ((eq field :custom-fields) (ds4e-item-tags-format (ds4e-item-custom-fields item) ", "))
          ((eq field :tags-and-fields) (ds4e-item-tags-format
                                        (append (ds4e-item-tags item)
                                                (ds4e-item-custom-fields item))
                                        ", ")))))

(defun ds4e-item-format (fmt item &optional datefmt sep no-join)
  "Format ITEM using a FMT list.

FMT is a list of keywords or strings. A keyword must be available
in `ds4e-item-field-info' or `ds4e-item-field-custom'. Keywords
are converted into their corresponding strings given the ITEM via
`ds4e-item-get-field' using DATEFMT andSEP, while strings are
passed as is. The list is finally joined into a single string,
unless NO-JOIN is t."
  (let ((strings (mapcar
                  (lambda (el)
                    (cond ((stringp el) el)
                          ((keywordp el) (ds4e-item-get-field item el datefmt sep))
                          (t "")))
                  fmt)))
    (if no-join
        strings
      (string-join strings))))

(provide 'ds4e-item)
;;; ds4e-item.el ends here
