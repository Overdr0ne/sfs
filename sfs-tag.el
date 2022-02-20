;;; sfs-tag.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/Overdr0ne>
;; Maintainer:  <scmorris.dev@gmail.com>
;; Created: June 19, 2020
;; Modified: June 19, 2020
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

(require 'dash)

(defun sfs-tag-collect-tags ()
  "Collect existing tags for file at POINT."
  (let (attr)
    (setq attr (-> (format "getfattr --absolute-names %s"
                           (shell-quote-argument (dired-get-filename)))
                   (shell-command-to-string)))
    (setq attr (dired-replace-in-string "#.*" "" attr))
    (split-string attr)))

(defun sfs-tag-collect-vals (tag)
  (-> (format "getfattr --absolute-names --only-values -n %s %s 2>/dev/null"
              tag (shell-quote-argument (dired-get-filename)))
      (shell-command-to-string)
      (split-string)))

(defun sfs-tag-set ()
  "Interactively tag the file at POINT using extended attributes."
  (interactive)
  (let ((tag (completing-read "Tag: " (sfs-tag-collect-tags))))
    (shell-command-to-string
     (format "setfattr -n %s -v %s %s" tag
	     (completing-read "Value: " (sfs-tag-collect-vals tag))
             (dired-get-filename)))))

(defun sfs-tag-get ()
  "Get all tag info for file at POINT using extended attributes."
  (interactive)
  (completing-read "Tag: " (sfs-tag-collect-tags)))

(provide 'sfs-tag)
;;; sfs-tag.el ends here
