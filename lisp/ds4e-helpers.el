;;; ds4e-helpers.el --- Part of ds4e, interface to Docspell

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
;;; This file contains helper functions

;;; Code:

(defun ds4e-result-get (result key &rest keys)
  "Get a value from RESULT using KEY.

Optionally specifying more KEYS will look into a nested
structure. The keys can be keywords or symbols."
  (let* ((key1 (car keys))
         (keyN (cdr keys))
         (keyS (if (keywordp key)
                   (intern (substring (symbol-name key) 1))
                 key))
         (res (alist-get keyS result)))
    ;; res is either scalar value or a list. for optional scalar
    ;; values it is also a list, either nil or containing a single
    ;; element
    ;;
    ;; so we can't distinguish between a single optional value or
    ;; possibly multi-value that contains only one element (like tags
    ;; with only one tag). A single element list will be unpacked,
    ;; because this case is more often.
    (if (and key1 res)
        (if (and (listp res) (not (cdr res)))
            (ds4e-result-get (car res) key1 keyN)
          (ds4e-result-get  res key1 keyN))
      res)))


(defun ds4e-basic-result (result)
  "Processes the 'BasicResult' structure RESULT.

Many commands return a flag indicating success or failure and a
message. Show the message if successful and raise a user error if
not."
  (if (ds4e-result-get result :success)
      (message "%s" (ds4e-result-get result :message))
    (user-error "%s" (ds4e-result-get result :message))))


(defun ds4e-mapcat (fun seq)
  "Apply FUN to all elements in SEQ and concatenate results."
  (apply 'append
         (mapcar fun seq)))


(defconst ds4e-log-buffer-name "*ds4e-out*"
  "Name of the log output buffer.")


(defun ds4e--make-output-buffer (name)
  "Get or create a buffer NAME for storing output."
  (unless (get-buffer name)
    (with-current-buffer (get-buffer-create name)
      (view-mode)
      (when (fboundp 'so-long-mode)
        (unless (eq major-mode 'so-long-mode)
          (so-long-mode)))
      (setq buffer-undo-list t)))
  name)

(defun ds4e--get-log-buffer ()
  "Get or create the log buffer."
  (ds4e--make-output-buffer ds4e-log-buffer-name))

(defun ds4e-log (level msg &rest args)
  "Log a message formatting MSG with ARGS to the log buffer.

LEVEL is a symbol defining the severity."
  (with-current-buffer (ds4e--get-log-buffer)
    (let* ((inhibit-read-only t)
           (ts (format-time-string "%Y-%m-%dT%T %Z"
                                   (current-time))))
      (save-excursion
        (goto-char (point-max))
        (insert "[" ts
                  " "
                  (symbol-name level)
                  " ds4e] "
                  (apply 'format msg args)
                  "\n")))))


(defun ds4e-format-unix-date (ts &optional fmt)
  "Format TS to local date with FMT."
  (if ts
      (format-time-string (or fmt "%Y-%m-%d") (/ ts 1000))
    "-"))
(provide 'ds4e-helpers)
;;; ds4e-helpers ends here
