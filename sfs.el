;;; sfs.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/sam>
;; Maintainer:  <scmorris.dev@gmail.com>
;; Created: May 23, 2020
;; Modified: May 23, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/sam/sfs
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

(defun sfs (query)
  (interactive "sQuery: ")
  (setq rslt (dbus-call-method
   :session
   "com.example.SampleService"
   "/SomeObject"
   "com.example.SampleInterface"
   "query"
   query))
  (setq urls (mapcar 'car rslt))
  (setq urls (remove-if-not (lambda (file) (file-exists-p file)) urls))
  (dired urls)
  )

(defun sfs-simple-any (query)
  (interactive "sQuery: ")
  (setq rslt (dbus-call-method
   :session
   "com.example.SampleService"
   "/SomeObject"
   "com.example.SampleInterface"
   "query"
   query))
  (setq urls (mapcar 'car rslt))
  (setq urls (remove-if-not (lambda (file) (file-exists-p file)) urls))
  (dired urls)
  )

(defun sfs-simple-all (query)
  (interactive "sQuery: ")
  (setq rslt (dbus-call-method
   :session
   "com.example.SampleService"
   "/SomeObject"
   "com.example.SampleInterface"
   "query"
   query))
  (setq urls (mapcar 'car rslt))
  (setq urls (remove-if-not (lambda (file) (file-exists-p file)) urls))
  (dired urls)
  )

(provide 'sfs)
;;; sfs.el ends here
