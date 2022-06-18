;;; ds4e-search.el --- Part of ds4e, interface to Docspell

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
;;; This file provides a search view that allows to query for
;;; documents and presents them in a special buffer.

;;; Code:

(require 'ds4e-settings)
(require 'ds4e-helpers)
(require 'ds4e-item)
(require 'hl-line)

;;; Settings

(defgroup ds4e-search nil
  "Settings for the search view."
  :group 'ds4e)

(defcustom ds4e-search-page-size
  60
  "The number of results to display at once in a buffer.

More results can be viewed by navigating to the next page."
  :type 'integer
  :group 'ds4e-search)

(defcustom ds4e-search-date-format "%Y-%m-%d"
  "Date format to use in search view."
  :type 'string
  :group 'ds4e-search)

(defcustom ds4e-search-group-position 'right
  "The position of the group name in the title.
This is a symbol from: 'begin, 'end, 'center. If nil, the title
is hidden and items are presented ungrouped."
  :type 'symbol
  :group 'ds4e-search)

(defcustom ds4e-search-columns
  '( (:date . 12)
     (:correspondent . 30)
     (:tags-and-fields . 30)
     (:name . nil))
  "A list of item fields to display and the amount of space to use.
Each element has the form (FIELD . WIDTH) where FIELD is on of
the available item fields (see `ds4e-item-field-info'). The last
element can have a width of nil, meaning unrestricted."
  :type 'alist
  :group 'ds4e-search)

(defcustom ds4e-search-default-query nil
  "A query that is set on start."
  :type 'string
  :group 'ds4e-search)

;;; Implementation

(defconst ds4e-search-buffer-name "*ds4e-search*"
  "Buffer name for the search view.")

(defvar ds4e-search-mode-hook nil)
(defvar ds4e-search-mode-map nil)

;; buffer local declaration
(defvar ds4e-search--offset)
(defvar ds4e-search--results)
(defvar ds4e-search--query)
(defvar ds4e-search--no-results)

(defun ds4e-search ()
  "Start the search view."
  (interactive)
  (let ((buffer (ds4e-search--make-buffer)))
    (with-current-buffer buffer
      (setq ds4e-search--offset 0)
      (ds4e-search-reload-view))
    (unless (eq (current-buffer) buffer)
      (pop-to-buffer buffer))))

(defun ds4e-search-reload-view (&optional offset)
  "Update view, optionally from OFFSET."
  (interactive)
  (let* ((inhibit-read-only t))
    (setq ds4e-search--offset (or offset 0))
    (ds4e-search--do-search)))

(defun ds4e-search-line-item-id ()
  "Return the item id of the item currently visiting."
  (plist-get (text-properties-at (line-beginning-position)) 'itemid))

(defun ds4e-search-line-item ()
  "Return the complete item currently visiting."
  (let* ((id (ds4e-search-line-item-id))
         (groups (ds4e-result-get ds4e-search--results :groups))
         (group (car groups))
         (found))
    (when id
      (while (and (not found) group)
        (setq found
              (seq-find
               (lambda (item)
                 (string-equal id (ds4e-item-id item)))
               (ds4e-result-get group :items)))
        (unless found
          (setq groups (cdr groups))
          (setq group (car groups))))
      found)))

(defun ds4e-search-goto-next-item ()
  "Move point to the next item."
  (interactive)
  (end-of-line)
  (search-forward-regexp "^item|"))

(defun ds4e-search-goto-prev-item ()
  "Move point to the next item."
  (interactive)
  (beginning-of-line)
  (search-backward-regexp "^item|"))

(defun ds4e-search-open-browser ()
  "Open the current item in the browser."
  (interactive)
  (let ((id (ds4e-search-line-item-id)))
    (when id
      (ds4e-client-open id))))

(defun ds4e-search-view-external ()
  "Run dsc to view files with external program."
  (interactive)
  (let ((id (ds4e-search-line-item-id)))
    (when id
      (ds4e-dsc-sync "view" (list :arg (concat "id:" id))))))

(defun ds4e-search-next-results ()
  "Increase offset and reload view."
  (interactive)
  (if ds4e-search--no-results
      (message "Already on last page.")
    (ds4e-search-reload-view
     (+ ds4e-search--offset ds4e-search-page-size))))

(defun ds4e-search-prev-results ()
  "Decrease offset and reload view."
  (interactive)
  (if (= 0 ds4e-search--offset)
      (message "Already on first page.")
    (ds4e-search-reload-view
     (max 0 (- ds4e-search--offset ds4e-search-page-size)))))

(defun ds4e-search-set-query ()
  "Set the query string and updpte view."
  (interactive)
  (let ((query (read-string "Query: " ds4e-search--query)))
    (setq ds4e-search--query query)
    (ds4e-search-reload-view)))

(defun ds4e-search-reset-query ()
  "Reset the query to an empty string and update view."
  (interactive)
  (setq ds4e-search--query "")
  (ds4e-search-reload-view))

(defun ds4e-search-set-bookmark ()
  "Ask for a bookmark and set it as query."
  (interactive)
  (let* ((bookmarks (ds4e-client-get-bookmarks))
         (name (completing-read "Bookmark: "
                                    (mapcar (lambda (bm)
                                              (ds4e-result-get bm :name))
                                            bookmarks)
                                    nil t))
         (selected (seq-find
                    (lambda (item)
                      (string-equal name (ds4e-result-get item :name)))
                    bookmarks)))
    (when selected
      (setq ds4e-search--query
            (ds4e-result-get selected :query))
      (setq ds4e-search--offset 0)
      (ds4e-search-reload-view))))

;;;

(defun ds4e-search--do-search ()
  "Run the search and update buffer."
  (ds4e-dsc-async
     "search"
     (list :limit ds4e-search-page-size
           :offset ds4e-search--offset
           :arg ds4e-search--query)
     (lambda (proc expr)
       (ds4e-log 'DEBUG "Result: %s" expr)
       (when (listp expr)
         (with-current-buffer (ds4e-search--make-buffer)
           (if (not (ds4e-result-get expr :groups))
               (progn
                 (setq ds4e-search--no-results t)
                 (message "No results!"))
             (erase-buffer)
             (setq ds4e-search--no-results nil)
             (setq ds4e-search--results expr)))
         (ds4e-search--apply-view expr)))
     nil
     (current-buffer)))

(defun ds4e-search--apply-view (result)
  "Append search RESULT to current view.
It must be non empty."
  (let* ((groups (ds4e-result-get result :groups))
         (fixedw (+ (apply '+
                           (mapcar
                            (lambda (e) (or (cdr e) 0))
                            ds4e-search-columns));; sum of fixed columns
                    (length ds4e-search-columns) ;; space between colums
                    3)))
    (with-current-buffer (ds4e-search--make-buffer)
      (goto-char (point-max))
      (dolist (group groups)
        (let ((name (ds4e-result-get group :name))
              (items (ds4e-result-get group :items)))
          (when ds4e-search-group-position
            (unless (eq (point) (point-min))
              (insert "\n"))
            (insert (propertize (ds4e-search--group-title name)
                                'face 'ds4e-search-title-face))
            (insert "\n"))
          (dolist (item items)
            (insert (ds4e-search--item-id item))
            (let* ((icons (cond
                           ((ds4e-item-created-p item) "⚐ ")
                           ((ds4e-item-due-p item) "⏲")
                           (t "  ")))
                   (cols (mapcar (lambda (field)
                                  (ds4e-search--item-value item field fixedw))
                                 ds4e-search-columns))
                   (line (string-join (cons icons cols) " ")))
              (if (ds4e-item-created-p item)
                  (insert (propertize line 'face 'ds4e-search-new-item-face) "\n")
                (insert line "\n"))))))
      (goto-char (point-min))
      (ds4e-search-goto-next-item))))

(defun ds4e-search--item-id (item)
  "Return an invisible string used to identify an ITEM line."
  (let* ((id (ds4e-item-id item))
         (pre (concat "item|" id "|")))
    (propertize pre 'itemid id 'invisible t)))

(defun ds4e-search--group-title (name)
  "Reformat NAME to be a header line."
  (let* ((totalw (- (window-total-width) 2))
        (namelen (+ 2 (length name)))
        (remain (- totalw namelen))
        (part (/ remain 2))
        (rest (% remain 2))
        (padchar (string-to-char "—")))
    (cond ((eq ds4e-search-group-position 'left)
           (concat
            (make-string 1 padchar)
            " "
            name
            " "
            (make-string (1- remain) padchar)))
          ((eq ds4e-search-group-position 'right)
           (concat
            (make-string (1- remain) padchar)
            " "
            name
            " "
            (make-string 1 padchar)))
          (t (concat ;;default to center
              (make-string part padchar)
              " "
              name
              " "
              (make-string (+ part rest) padchar))))))

(defun ds4e-search--item-value (item field fixedw)
  "Return the value of the given FIELD in ITEM."
  (let* ((column (car field))
         (totalw (window-total-width))
         (width (or (and (cdr field) (1- (cdr field)))
                    (- totalw fixedw)))
         (value (ds4e-item-get-field item column ds4e-search-date-format "/")))
    (ds4e-search--fill value width "-")))

(defun ds4e-search--header-line-format ()
  "Create the format for the headerline."
  (let ((field-defs (append ds4e-item-field-custom ds4e-item-field-info)))
    (cons (make-string (+ 3 (floor (fringe-columns 'left t))) ?\s)
          (mapcar
           (lambda (item)
             (let* ((field (car item))
                    (info (cdr (assoc field field-defs)))
                    (help (plist-get info :help))
                    (width (cdr item))
                    (name (plist-get info :name)))
               (propertize
                (if width
                      (ds4e-search--fill name width)
                    name)
                'face 'ds4e-search-header-face
                'help-echo help)))
           ds4e-search-columns))))

(defun ds4e-search--make-buffer ()
  "Create the search buffer."
  (let ((buf (get-buffer ds4e-search-buffer-name)))
    (unless buf
      (setq buf (get-buffer-create ds4e-search-buffer-name))
      (with-current-buffer  buf
        (ds4e-search-mode)))
    buf))

(defun ds4e-search--fill (str width &optional default)
  "Return STR with exact size WIDTH.
It is filled with space or cut depending on size of STR. If STR
is nil, return DEFAULT. If WIDTH is nil, retur STR as is."
  (let* ((val (or str default))
         (len (length val)))
    (if width
        (cond ((= len width) val)
              ((< len width) (concat
                              val
                              (make-string (- width len) 32)))
              (t (concat (substring val 0 (1- width)) "…")))
      val)))

(define-derived-mode ds4e-search-mode special-mode
  "Docspell:Search"
  "Mode for displaying docspell search results.
\\{ds4e-search-mode-map}"

  (kill-all-local-variables)
  (use-local-map ds4e-search-mode-map)

  (setq buffer-read-only t
        truncate-lines t
        buffer-undo-list t
        overwrite-mode nil
        header-line-format (ds4e-search--header-line-format))

  (set (make-local-variable 'ds4e-search--query) (or ds4e-search-default-query ""))
  (set (make-local-variable 'ds4e-search--offset) 0)
  (set (make-local-variable 'ds4e-search--results) nil)
  (set (make-local-variable 'ds4e-search--no-results) nil)

  (set (make-local-variable 'hl-line-face) 'ds4e-search-highlight-face)
  (hl-line-mode 1))

(unless ds4e-search-mode-map
  (setq ds4e-search-mode-map (make-sparse-keymap))
  (suppress-keymap ds4e-search-mode-map)
  (define-key ds4e-search-mode-map (kbd "q") 'quit-window)
  (define-key ds4e-search-mode-map (kbd "g") 'ds4e-search-reload-view)
  (define-key ds4e-search-mode-map (kbd "n") 'ds4e-search-goto-next-item)
  (define-key ds4e-search-mode-map (kbd "p") 'ds4e-search-goto-prev-item)
  (define-key ds4e-search-mode-map (kbd "s") 'ds4e-search-set-query)
  (define-key ds4e-search-mode-map (kbd "b") 'ds4e-search-set-bookmark)
  (define-key ds4e-search-mode-map (kbd "R") 'ds4e-search-reset-query)
  (define-key ds4e-search-mode-map (kbd "M-n") 'ds4e-search-next-results)
  (define-key ds4e-search-mode-map (kbd "M-p") 'ds4e-search-prev-results)
  (define-key ds4e-search-mode-map (kbd "RET") 'ds4e-search-view-external)
  (define-key ds4e-search-mode-map (kbd "SPC") 'scroll-up-line))

(provide 'ds4e-search)
;;; ds4e-search.el ends here
