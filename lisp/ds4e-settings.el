;;; ds4e-settings.el --- Part of ds4e, interface to Docspell

;; Copyright © 2022- Eike Kettner

;; This file is not part of GNU Emacs.

;;; Commentary:
;;;
;;; Some global vars.

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

;;; Code:

(defconst ds4e-version "unknown"
  "The version of ds4e.")

(defgroup ds4e nil
  "Interface to docspell."
  :group 'external)


(defgroup ds4e-faces nil
  "Type faces used in ds4e."
  :group 'ds4e
  :group 'faces)

(defface ds4e-search-title-face
  '((t :inherit font-lock-type-face))
  "Face for a group title in the search view."
  :group 'ds4e-faces)

(defface ds4e-search-header-face
  '((t :inherit font-lock-comment-face))
  "Face for the header line in the search view."
  :group 'ds4e-faces)

(defface ds4e-search-new-item-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for an new item."
  :group 'ds4e-faces)

(defface ds4e-search-highlight-face
  `((t :inherit hl-line :weight bold :underline nil
       ,@(and (>= emacs-major-version 27) '(:extend t))))
  "Face for the header at point."
  :group 'ds4e-faces)

(provide 'ds4e-settings)
;;; ds4e-settings.el ends here
