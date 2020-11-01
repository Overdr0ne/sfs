;;; sfs-index.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/sam>
;; Maintainer:  <scmorris.dev@gmail.com>
;; Created: August 10, 2020
;; Modified: August 10, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/sam/sfs-index
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(defun sfs-recoll-index ()
  "Start the recoll indexer."
  (interactive)
  (async-shell-command (concat "recollindex -c "
                               (getenv "RECOLL_CONFDIR"))
                       "*recollindex output*"
                       "*recollindex error"))

(defun sfs-recoll-config ()
  "Open the recoll configuration file, interactively creating one if necessary."
  (interactive)
  (let (config)
    (when (not (setq config (getenv "RECOLL_CONFDIR")))
        (progn
          (message "Set RECOLL_CONFDIR to the directory with your recoll.conf, e.g. ~/.recoll.")
          (setq config (concat (getenv "HOME") "/.recoll"))))
    (find-file (concat config "/recoll.conf"))))

(defun sfs-recoll-monitor ()
  "Start the monitoring files for indexing with recoll."
  (async-shell-command "recollindex -m"))

(provide 'sfs-index)
;;; sfs-index.el ends here
