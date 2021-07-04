;;; sfs.el --- SFS - Search File-System  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Keywords: tools, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(require 'sfs-reindex)
(require 'sfs-recoll)
(require 'sfs-tag)
(require 'sfs-tui)

(defvar sfs--initialized-p nil)

(defun sfs--init ()
  "Starts the SFS services if necessary."
  (when (not sfs--initialized-p)
    (let ((service-filename
           (concat (file-name-directory (file-name-directory (find-library-name "sfs"))) "service.py")))
      (if (file-exists-p service-filename)
          (start-process
           "recoll-server"
           "*recoll-server*"
           "python3"
           service-filename)
        (message "SFS: Couldn't find service.py...")))
    (setq sfs--initialized-p t)))

(defun sfs--shutdown ()
  "Shuts down SFS services when necessary."
  (when sfs--initialized-p
    (kill-process "recoll-server")
    (setq sfs--initialized-p nil)))

(define-minor-mode global-sfs-mode
  "Toggle global SFS minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When enabled, SFS initializes a python server responsible for
dispatching queries from elisp over dbus, and returning search results."
  :init-value nil
  :lighter " SFS"
  :global t
  (if global-sfs-mode
      (sfs--init)
    (sfs--shutdown)))

(provide 'sfs)
;;; sfs.el ends here
