;;; ds4e-dsc.el --- Part of ds4e, interface to Docspell

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
;;; Lower level functions calling the dsc program.

;;; Code:
(require 'ds4e-helpers)

(defcustom ds4e-dsc-executable
  (executable-find "dsc")
  "Name of the dsc program.

If it is not in your $PATH, specify a full path."
  :type '(file :must-match t)
  :safe 'stringp
  :group 'ds4e)

(defcustom ds4e-dsc-config
  nil
  "A config file to use when running dsc.

The format option will always be overriden to return sexps, other
values can be defined in a separate config file."
  :type '(file :must-match t)
  :safe 'stringp
  :group 'ds4e)

(defcustom ds4e-dsc-server-url
  nil
  "Allow to override the docspell server url.

This takes precedence over any value defined in the config file."
  :type 'tring
  :safe 'stringp
  :group 'ds4e)

(defcustom ds4e-dsc-verbose
  0
  "The verbosity when running dsc. A number out of: 0, 1, 2.

Log messages are printed to stderr. The stderr output of dsc is
loaded into the buffer *ds4e-out* to be inspected."
  :type 'integer
  :group 'ds4e)


;;; Public functions

(defun ds4e-dsc-async (subcmd arg-plist callback &optional cwd buffer-or-name)
  "Run dsc SUBCMD asynchronously with ARG-PLIST invoking CALLBACK.

The process is executed in CWD and stderr output is passed to the
log buffer. The process is tight to BUFFER-OR-NAME if present."
  (ds4e-dsc-async-sexp
   (ds4e-dsc--make-command-args subcmd arg-plist)
   callback
   cwd
   buffer-or-name))

(defun ds4e-dsc-sync (subcmd arg-plist &optional cwd)
  "Run dsc SUBCMD synchronously with ARG-PLIST returning the output.

The process is run in CWD if given. The first sexp from stdout is
returned."
  (ds4e-dsc-sync-sexp (ds4e-dsc--make-command-args subcmd arg-plist) cwd))



;;; Internal functions

(defvar ds4e-dsc-display-error-buffer nil
  "When t, the log buffer is be displayed on error.")

(defvar ds4e-dsc-callback)

(defvar ds4e-dsc-kill-parse-buffer t
  "Whether to kill the temporary parse buffer.
Can be set to nil for debugging purposes.")

;; buffer local variable declaration
(defvar ds4e-dsc-proc-data)


(defun ds4e-dsc-async-sexp (cmd callback &optional cwd buffer-or-name)
  "Run dsc in CWD with arguments CMD expecting s-exp results.

Start a process using CMD, which is a list containing the
arguments to the dsc executable. The output is collected and
expected to be s-expressions. The sexps are read and CALLBACK is
invoked with two arguments: the process object and one sexp from
the output.

The stderr output is appended to the log buffer, which is reset
on each invocation."
  (let ((procbuf (or buffer-or-name
                     (ds4e--make-output-buffer "*ds4e-dsc-proc*")))
        (logbuf (ds4e--get-log-buffer)))
    (let ((proc (get-buffer-process procbuf)))
      (when proc
        (if (or (not (eq (process-status proc) 'run))
                (yes-or-no-p "A `dsc' process is running. Kill it? "))
            (condition-case nil
                (progn
                  (interrupt-process proc)
                  (sit-for 1)
                  (delete-process proc))
              (error nil))
          (user-error "Cannot run two dsc processes in `%s' at onec" procbuf))))
    (with-current-buffer logbuf
      (let ((inhibit-read-only t))
        (erase-buffer)))

    (with-current-buffer procbuf
      (let* ((proc)
             (ccmd (append '("-f" "elisp") cmd))
             (kmap (make-sparse-keymap)))
        (setq default-directory (or (and (stringp cwd) cwd) default-directory))
        (unless (boundp 'ds4e-dsc-proc-data)
          (set-keymap-parent kmap (current-local-map))
          (define-key kmap (kbd "C-c C-k") 'ds4e-dsc-kill)
          (use-local-map kmap))
        (set (make-local-variable 'ds4e-dsc-proc-data)
             (list :callback callback
                   :parse-buffer (get-buffer-create
                                  (format "%s-%s"
                                          (buffer-name procbuf)
                                          "read"))))
        (setq proc (make-process
                    :name "dsc"
                    :buffer procbuf
                    :command (cons ds4e-dsc-executable ccmd)
                    :filter 'ds4e-dsc--sexp-filter
                    :sentinel 'ds4e-dsc--sexp-sentinel
                    :stderr (make-pipe-process
                             :name "dsc"
                             :buffer nil
                             :filter 'ds4e-dsc--stderr-filter)))
        (with-current-buffer (plist-get ds4e-dsc-proc-data :parse-buffer)
          (move-marker (process-mark proc) (point))
          (erase-buffer))
        (setq mode-line-process '(":%s"))
        proc))))

(defun ds4e-dsc--stderr-filter (proc out)
  "Filter function for stderr for PROC.
It appends OUT to the log buffer."
  (with-current-buffer (ds4e--get-log-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert out)))))


(defun ds4e-dsc--sexp-filter (proc out)
  "The process filter function for PROC `ds4e-dsc-async-sexp'.

It uses the temporary buffer `ds4e-dsc-parse-buffer' to
accumulate OUT from stdout and tries to parse sexps from it."
  (let* ((going t)
         (inhibit-read-only t)
         (procdata
          (with-current-buffer (process-buffer proc)
            ds4e-dsc-proc-data)))
    (with-current-buffer (plist-get procdata :parse-buffer)
      (save-excursion
        (save-restriction
          (goto-char (point-max))
          (insert out)
          (while going
            (let* ((pos (marker-position (process-mark proc)))
                   (result (condition-case nil
                               (read-from-string
                                (buffer-substring pos (point)))
                             (error nil))))
              (when (>= ds4e-dsc-verbose 2)
                (ds4e-log 'DEBUG "Read output (%s-%s): %s" pos (point) result))
              (when (car result)
                (move-marker (process-mark proc) (+ pos (cdr result)))
                (funcall (plist-get procdata :callback) proc (car result)))
              (setq going result))))))))

(defun ds4e-dsc--sexp-sentinel (proc state)
  "The sentinel function for `ds4e-dsc-async-sexp'.

Called when process PROC terminates with STATE."
  (let ((parsebuf (with-current-buffer (process-buffer proc)
                    (plist-get ds4e-dsc-proc-data :parse-buffer)))
        (exit-status (process-exit-status proc)))
    (ds4e-log 'INFO "dsc %s" state)
    (delete-process proc)
    (force-mode-line-update)
    (when ds4e-dsc-kill-parse-buffer
      (kill-buffer parsebuf))
    (when (> exit-status 0)
      (when ds4e-dsc-display-error-buffer
        (display-buffer (ds4e--get-log-buffer)))
      (user-error "Dsc %s" (string-trim state)))))

(defun ds4e-dsc-kill ()
  "Kill the `dsc' process running in the current buffer."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (and proc (eq (process-status proc) 'run)
         (eq (process-filter proc) (function ds4e-dsc--sexp-filter))
         (condition-case nil
             (delete-process proc)
           (error nil)))))

(defun ds4e-dsc-sync-sexp (cmd &optional cwd)
  "Synchronously execute dsc in CWD.

CMD is a list of arguments to the dsc executable. Read the output
into a list of sexps and return it. It reads only one expression
from the result."
  (let ((ccmd (append '("-f" "elisp") cmd))
        (tmperr (make-temp-file "ds4e-temp-")))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory (or (and (stringp cwd) cwd) default-directory))
          (let ((rc (apply 'call-process ds4e-dsc-executable nil (list t tmperr) nil ccmd))
                (buferr (ds4e-dsc--load-error tmperr)))
            (if (= rc 0)
                (car (read-from-string (buffer-string)))
              (when ds4e-dsc-display-error-buffer
                (display-buffer buferr))
              (user-error "Dsc returned non-zero. See *ds4e-out* buffer for details"))))
      (delete-file tmperr))))

(defun ds4e-dsc--load-error (file)
  "Load FILE into the error buffer that is returned."
  (with-current-buffer (ds4e--get-log-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-file-contents file)
      (current-buffer))))

(defun ds4e-dsc--plist-to-args (plist)
  "Convert a keyword based PLIST into an argument list for dsc.

To have a more lispy style of calling dsc, this function converts
plists using colon keywords into an argument list that can be
used to run dsc. For example:

   '(:c mycoll :matches \"*\" :traverse t :arg \"file.pdf\")

is converted into

   '(\"-c\" \"mycoll\" \"--matches\" \"*\" \"--traverse\" \"file-pdf\")

A 't' value will result in a flag, that is an option without
value. A 'nil' value results in the option being ignored. A key
`:arg` renders an argument, that is the option name is ignored."
  (let ((alist (seq-partition plist 2)))
    (ds4e-mapcat (lambda (pair)
               (let* ((key (car pair))
                      (val (cadr pair))
                      (valn (cond
                             ((symbolp val) (symbol-name val))
                             ((stringp val) val)
                             ((numberp val) (number-to-string val))
                             (t (error
                                 (format "Can't format argument %s of type: %s" val (type-of val))))))
                      (opt (substring (symbol-name key) 1))
                      (optn (if (eq 1 (length opt))
                                (format "-%s" opt)
                              (format "--%s" opt))))
                 (cond ((eq val nil) nil)
                       ((eq key :arg) (list valn))
                       ((eq val 't) (list optn))
                       (t (list optn valn)))))
             alist)))

(defun ds4e-dsc--make-command-args (subcmd arg-plist)
  "Append SUBCMD and ARG-PLIST to the final command arguments."
  (append
   (cond ((= 1 ds4e-dsc-verbose) '("-v"))
         ((= 2 ds4e-dsc-verbose) '("-vv"))
         (t nil))
   (ds4e-dsc--plist-to-args (list :config ds4e-dsc-config :docspell-url ds4e-dsc-server-url))
   (list subcmd)
   (ds4e-dsc--plist-to-args arg-plist)))

(provide 'ds4e-dsc)
;;; ds4e-dsc.el ends here
