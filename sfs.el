;;; sfs --- Search File System: A search engine for your filesystem
;;; sfs.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/Overdr0ne>
;; Maintainer:  <http://github/Overdr0ne>
;; Created: May 23, 2020
;; Modified: May 24, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/Overdr0ne/sfs
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'dbus)

(defun sfs-call (method &rest args)
  "Send METHOD to the SFS python server with optional ARGS."
  (dbus-call-method
   :session
   "com.sfs.SearchService"
   "/SFS"
   "com.sfs.SearchInterface"
   method
   args))

(defun sfs (queryStr)
  "Query the Recoll database and display the results in dired.
QUERYSTR is a search string conforming to the Recoll Query Language."
  (interactive "sQuery: ")
  (let ((rslt nil)
        (urls nil))
    (setq rslt (dbus-call-method
                :session
                "com.sfs.SearchService"
                "/SFS"
                "com.sfs.SearchInterface"
                "Query"
                queryStr))
    (setq urls (mapcar 'car rslt))
    (setq urls (remove-if-not (lambda (file) (file-exists-p file)) urls))
    (dired urls)))

(defun sfs-exit ()
  "Exit SFS Recoll server."
  (interactive)
  (dbus-call-method
   :session
   "com.sfs.SearchService"
   "/SFS"
   "com.sfs.SearchInterface"
   "Exit"))

(start-process
 "recoll-server"
 "*recoll-server*"
 "python"
 (concat (file-name-directory load-file-name) "service.py"))

(provide 'sfs)
;;; sfs.el ends here
