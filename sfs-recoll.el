;;; sfs-recoll --- An sfs interface to recoll.
;;; sfs-recoll.el -*- lexical-binding: t; -*-
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
(require 'ivy)
(require 'dired)

(defun sfs--recoll-get-field (field aList)
  "Utility function to get FIELD from ALIST."
  (cadr (assoc field aList)))

(defun sfs--recoll-get-fields (fields aList)
  "Utility function to get FIELDS from ALIST."
  (mapcar #'(lambda (field) (sfs--recoll-get-field field aList))
          fields))

(defun sfs--recoll-collect-fields (fields db)
  "Collect data FIELDS from DB."
  (mapcar #'(lambda (entry) (car (sfs--recoll-get-fields fields entry)))
          db))

(defun sfs--recoll-property-str (entry)
  "Build tooltip string from DBENTRY."
  (let (infoStrs)
    (setq entry
          (remove-if #'(lambda (field) (string-empty-p (cadr field)))
                     entry))
    (setq infoStrs (mapcar
                    #'(lambda (field)
                        (concat (car field) ": " (cadr field) "\n"))
                    entry))
    (reduce #'concat infoStrs)))

(defun sfs--recoll-search-md5 (filename)
  (let (buf-str query entry)
    (setq buf-str (with-temp-buffer
                    (insert-file-contents filename)
                    (buffer-string)))
    (setq query (concat "rclmd5:"
                        (secure-hash 'md5 buf-str)))
    (car (sfs--recoll-search query))))

(defun sfs--recoll-search-filename (filename)
  (let (query split dirs name)
    (setq split (split-string filename "/"))
    (setq split (remove-if #'(lambda (str) (string= str ""))
                           split))
    (setq name (car (last split)))
    (setq dirs (butlast split))
    (setq query (mapcar #'(lambda (dir) (concat "dir:" dir))
                        dirs))
    (add-to-list 'query (concat "filename:" name))
    (setq query (string-join query " AND "))
    (car (sfs--recoll-search query))))

(defun sfs--recoll-file-properties (filename)
  "Get recoll properties for FILENAME by looking up its md5 hash or guessing from the filename."
  (let (entry)
    (setq entry
          (-some #'(lambda (x) x)
                 `(,(sfs--recoll-search-md5 filename)
                   ,(progn
                      (message "Couldn't find entry based on it's hash, guessing the entry based on filename...")
                      (sfs--recoll-search-filename filename)))))))

(defun sfs--recoll-call (methodStr &rest args)
  "Send METHOD to the SFS python server with optional ARGS."
  ;; TODO only works with one argument
  (dbus-call-method
   :session
   "com.sfs.SearchService"
   "/SFS"
   "com.sfs.SearchInterface"
   methodStr
   (car args)))

(defun sfs--recoll-search (query-str)
  (sfs--recoll-call "Query" query-str))

(defun sfs--recoll-find-urls (query-str)
  "Query the Recoll database and display the results in dired.
QUERYSTR is a search string conforming to the Recoll Query Language."
  (if (not (string= query-str ""))
      (let ((raw-db (sfs--recoll-search query-str)))
        (if raw-db
            (let ((db (remove-if-not #'(lambda (entry)
                                         (file-exists-p (sfs--recoll-get-field "url"
                                                                               entry)))
                                     raw-db)))
              (sfs--recoll-collect-fields '("url") db))
          (message "SFS: Couldn't find any results for your search...")))
    (message "SFS: query string empty...")))

(defun sfs-recoll-exit ()
  "Exit SFS Recoll server."
  (interactive)
  (dbus-call-method
   :session
   "com.sfs.SearchService"
   "/SFS"
   "com.sfs.SearchInterface"
   "Exit"))

(provide 'sfs-recoll)
;;; sfs-recoll.el ends here
