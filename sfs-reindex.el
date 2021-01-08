;;; sfs-index.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/Overdr0ne>
;; Maintainer:  <scmorris.dev@gmail.com>
;; Created: August 10, 2020
;; Modified: August 10, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/Overdr0ne/sfs
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(defun sfs-reindex-build ()
  "Start the recoll indexer."
  (interactive)
  (async-shell-command (concat "recollindex -c "
                               (getenv "RECOLL_CONFDIR"))
                       "*recollindex output*"
                       "*recollindex error"))

(defun sfs-reindex-config ()
  "Open the recoll configuration file, interactively creating one if necessary."
  (interactive)
  (when (not (getenv "RECOLL_CONFDIR"))
    (progn
      (message "RECOLL_CONFDIR not set. Setting it to ~/.recoll.")
      (setenv "RECOLL_CONFDIR" (concat (getenv "HOME") "/.recoll"))
      (when (not (file-exists-p (getenv "RECOLL_CONFDIR")))
        (mkdir (getenv "RECOLL_CONFDIR")))))
  (find-file (concat (getenv "RECOLL_CONFDIR") "/recoll.conf")))

(defun sfs-reindex-monitor ()
  "Start the monitoring files for indexing with recoll."
  (async-shell-command "recollindex -m"))

(provide 'sfs-reindex)
;;; sfs-index.el ends here
