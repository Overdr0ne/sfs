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

(define-minor-mode sfs-dired-mode
  "Get your foos in the right places."
  :lighter " sfs-dired"
  :keymap (make-sparse-keymap))

(defvar dbIndex nil)
(make-variable-buffer-local 'dbIndex)

(defun sfs-recoll-get-field (field aList)
  "Utility function to get FIELD from ALIST."
  (cadr (assoc field aList)))

(defun sfs-recoll-get-fields (fields aList)
  "Utility function to get FIELDS from ALIST."
  (mapcar #'(lambda (field) (sfs-recoll-get-field field aList))
          fields))

(defun sfs-recoll-collect-fields (fields db)
  "Collect data FIELDS from DB."
  (mapcar #'(lambda (entry) (car (sfs-recoll-get-fields fields entry)))
          db))

(defun sfs-recoll-build-tooltip (dbEntry)
  "Build tooltip string from DBENTRY."
  (let ((dbInfo nil)
        (infoStrs nil))
    (setq dbInfo (cdr dbEntry))
    (setq dbInfo
          (remove-if #'(lambda (field) (string-empty-p (cadr field)))
                     dbInfo))
    (setq infoStrs (mapcar
                    #'(lambda (field)
                        (concat (car field) ": " (cadr field) "\n"))
                    dbInfo))
    (reduce #'concat infoStrs)))

(defun sfs-display-info-hook (entry)
  (let ((buffer (get-buffer-create " *Minibuf*")) str)
    (setq str (sfs-recoll-build-tooltip entry))
    (with-current-buffer buffer
      (visual-line-mode 1)
      (auto-fill-mode 1)
      (erase-buffer)
      (insert str)
      (display-buffer buffer)))
  (select-window (get-mru-window)))

(defun sfs-display-info ()
  "Display a tooltip showing metadata about the file at POINT."
  (interactive)
  (let (entry)
    (setq entry (assoc (dired-get-filename) dbIndex))
    (minibuffer-with-setup-hook
        (lambda () (sfs-display-info-hook entry))
      (read-string "Enter: "))))

(defmacro minibuffer-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

(defun sfs-dired-next-line ()
  (interactive)
  "Go to next line, and display file info in minibuffer."
  (minibuffer-quit-and-run
   (dired-next-line 1)
   (sfs-display-info)))
;; (substitute-key-definition 'dired-next-line 'sfs-dired-next-line dired-mode-map)

(defun sfs-dired-previous-line ()
  (interactive)
  "Go to previous line, and display file info in minibuffer."
  (minibuffer-quit-and-run
   (dired-previous-line 1)
   (sfs-display-info)))
;; (substitute-key-definition 'dired-previous-line 'sfs-dired-previous-line dired-mode-map)

(defun sfs-recoll-tooltip ()
  "Display a tooltip showing metadata about the file at POINT."
  (interactive)
  (let (entry)
    (setq entry (assoc (dired-get-filename) dbIndex))
    (popup-tip (sfs-recoll-build-tooltip entry) :point(line-beginning-position))))

(defun sfs-recoll-get-ivy-alist (dbEntry)
  "Build alist for ivy DBENTRY."
  (let (dbInfo infoStrs)
    (setq dbInfo (cdr dbEntry))
    (setq dbInfo
          (remove-if #'(lambda (field) (string-empty-p (cadr field)))
                     dbInfo))
    (setq infoStrs
          (mapcar #'(lambda (field) (cons (concat (car field) ": " (cadr field)) (cadr field)))
                  dbInfo))))

(defun sfs-recoll-dired-ivy ()
  "Operate on file metadata at POINT."
  (interactive)
  (let (entry)
    (setq entry
          (-> (dired-get-filename)
             (assoc dbIndex)
             (sfs-recoll-get-ivy-alist)))
    (ivy-read "Find Property: " entry
              :caller 'sfs-recoll-dired-ivy
              :action
              #'(lambda (a) (progn
                         (kill-new (cdr a))
                         (print (concat "Entry\n---\n" (cdr a) "\n---\ncopied to kill-ring.")))))))

(defun sfs-recoll-build-dired-index (db)
  "Build an alist for the DB using the file url as key."
  (mapcar #'(lambda (entry) (cons (sfs-recoll-get-field "url" entry) entry))
          db))

(defun sfs-recoll-call (method &rest args)
  "Send METHOD to the SFS python server with optional ARGS."
  (dbus-call-method
   :session
   "com.sfs-recoll.SearchService"
   "/SFS"
   "com.sfs-recoll.SearchInterface"
   method
   args))

(defun sfs-recoll (queryStr)
  "Query the Recoll database and display the results in dired.
QUERYSTR is a search string conforming to the Recoll Query Language."
  (interactive "sQuery: ")
  (let (rawDB db index urls sfs-recollBuf)
    (setq rawDB (dbus-call-method
                 :session
                 "com.sfs.SearchService"
                 "/SFS"
                 "com.sfs.SearchInterface"
                 "Query"
                 queryStr))
    (if rawDB
        (progn (setq db
                     (remove-if-not #'(lambda (entry) (file-exists-p (sfs-recoll-get-field "url" entry)))
                                    rawDB))
               (setq index (sfs-recoll-build-dired-index db))
               (setq urls (sfs-recoll-collect-fields '("url") db))
               (setq sfs-recollBuf (dired urls))
               (set-buffer sfs-recollBuf)
               (setq dbIndex index)
               (sfs-dired-mode) )
      (message "SFS: Couldn't find any results for your search..."))))

(defun sfs-recoll-exit ()
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
 "python3"
 (concat (file-name-directory load-file-name) "service.py"))

(provide 'sfs-recoll)
;;; sfs-recoll.el ends here
