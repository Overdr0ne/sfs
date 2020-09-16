;;; sfs-collections.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Sam
;;
;; Author: Sam <http://github/sam>
;; Maintainer: Sam <scmorris.dev@gmail.com>
;; Created: September 16, 2020
;; Modified: September 16, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/sam/sfs-collections
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(defvar sfs-favs nil)

(defun sfs-make-section-favs ()
  "Make the favorites section in the sfs TUI."
  (sfs--insert-section-heading "Favorites")
  (dolist (elmt sfs-favs)
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (sfs-recoll (string-join (cadr elmt) " ")))
                   (car elmt))
    (widget-insert " ")))

(defun sfs--make-search-tui ()
  "Write the tui buffer."
  (let ((buffer (switch-to-buffer "*SFS Collections*")))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (remove-overlays)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (sfs-insert-logo)
      (widget-insert "\n\n")
      (sfs-make-section-favs)
      (sfs-mode)
      (keyboard-escape-quit)
      (widget-setup))))

(provide 'sfs-collections)
;;; sfs-collections.el ends here
