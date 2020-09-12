;;; sfs-tui.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/sam>
;; Maintainer:  <scmorris.dev@gmail.com>
;; Created: July 06, 2020
;; Modified: July 06, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/sam/sfs
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'sfs-recoll)

(defvar sfs-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "RET") 'sfs-ex-query)
    (define-key map (kbd "a") 'sfs-author)
    (define-key map (kbd "c") 'sfs-containerfilename)
    (define-key map (kbd "d") 'sfs-dir)
    (define-key map (kbd "e") 'sfs-ext)
    (define-key map (kbd "f") 'sfs-filename)
    (define-key map (kbd "h") '(lambda () (interactive) (which-key-show-keymap 'sfs-mode-map)))
    (define-key map (kbd "i") 'sfs-simple)
    (define-key map (kbd "j") 'sfs-subject)
    (define-key map (kbd "k") 'sfs-keyword)
    (define-key map (kbd "r") 'sfs-recipient)
    (define-key map (kbd "l") 'sfs-rclcat)
    (define-key map (kbd "m") 'sfs-mime)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "s") 'sfs-size)
    (define-key map (kbd "t") 'sfs-date)
    (define-key map (kbd "x") 'sfs-ex-query)
    map)
  "Keymap for `sfs-mode'.")

(define-derived-mode sfs-mode special-mode "SFS"
  "Mode for SFS interactive search interface."
  (font-lock-mode -1)
  )
;; :lighter " SFS"
;; :keymap sfs-mode-map)

;; (add-to-list 'emulation-mode-map-alists `((sfs-mode . ,sfs-mode-map)))
;; (setq emulation-mode-map-alists (cons `((sfs-mode . ,sfs-mode-map)) emulation-mode-map-alists))
;; (setq emulation-mode-map-alists nil)

(defun sfs-insert-logo ()
  "Make SFS logo in search buffer."
  (widget-insert (f-read-text "./assets/graffiti.txt"))
  )

(cl-defstruct (sfs-field (:constructor sfs-field-create (label collector desc))
                         (:copier nil))
  label collector desc prompt prefix)

(defvar sfs-fields
  `( ,(sfs-field-create "(a)uthor" #'sfs-author "looks for AUTHOR in Recoll's extracted metadata")
     ,(sfs-field-create "(c)ontainer" #'sfs-containerfilename "filename set for both top-level and subdocuments")
     ,(sfs-field-create "(d)irectory" #'sfs-dir "full or relative, tildes and wildcards expanded, positive or negative, e.g. bin/ -~/")
     ,(sfs-field-create "(e)xtension" #'sfs-ext "filename extension")
     ,(sfs-field-create "(f)ilename" #'sfs-filename "not necessarily set for compound documents, e.g. those in an EPUB section")
     ,(sfs-field-create "(h)elp" (lambda () (interactive) (which-key-show-keymap 'sfs-mode-map)) "Shows which-key help menu")
     ,(sfs-field-create "s(i)mple" #'sfs-simple "generic google-like search query")
     ,(sfs-field-create "sub(j)ect" #'sfs-subject "subject/title/caption of the document from extracted metadata")
     ,(sfs-field-create "(k)eyword" #'sfs-keyword "document-specified keywords(few documents actually have any)")
     ,(sfs-field-create "(r)ecipient" #'sfs-recipient "looks for RECIPIENT in Recoll's extracted metadata")
     ,(sfs-field-create "rc(l)cat" #'sfs-rclcat "e.g. text or -presentation etc.")
     ,(sfs-field-create "(m)ime type" #'sfs-mime "e.g. text/plain or text/* etc.")
     ,(sfs-field-create "(s)ize" #'sfs-size "e.g. >1000k or <30G or =10g etc.")
     ,(sfs-field-create "da(t)e" #'sfs-date "YYYY-MM-DD/YYYY-MM-DD or YYYY/ or /YYYY-MM or YYYY etc.")
     ))

(defun sfs-make-section-fields ()
  "Make the fields section in the search buffer."
  (let ()
    (widget-insert "Fields:\n")
    (push-mark)
    (mapcar
     (lambda (field)
       (widget-create 'push-button
                      :notify (lambda (&rest ignore)
                                sfs-author
                                (sfs--gui-run (sfs-field-collector field))
                                )
                      (sfs-field-label field)
                      )
       (widget-insert (concat "\t: " (sfs-field-desc field) "\n"))
       )
     sfs-fields)
    (let ((inhibit-read-only t))
      (pop-mark)
      (align-regexp (mark) (point) "\\(\\s-*\\):"))
    )
  )

(defvar sfs-query nil)

(defun sfs-make-section-query ()
  "Make the query section in sfs gui."
  (widget-insert "Query:\n")
  (dolist (elmt sfs-query)
    (widget-insert (concat elmt "\n")))
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (sfs-ex-query))
                 "e(x)ecute"))

(defun sfs--update-gui ()
  "Write the gui buffer."
  (let ((buffer (switch-to-buffer "*SFS*")))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (remove-overlays)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (sfs-insert-logo)
      (widget-insert "\n\n")
      (sfs-make-section-query)
      (widget-insert "\n\n")
      (sfs-make-section-fields)
      (sfs-mode)
      (keyboard-escape-quit)
      (widget-setup)
      ))
  )

(defun sfs--gui-run (f)
  "Call sfs query F in GUI context."
  (call-interactively f)
  (sfs--update-gui))

(defun sfs ()
  "Go to SFS start page."
  (interactive)
  (sfs--update-gui)
  )

(defun sfs-ex-query ()
  "Execute search query."
  (interactive)
  (sfs-recoll (string-join sfs-query " ")))

(defun sfs-split-and-prefix (prefix query)
  "Split the QUERY on space, and prepend the provided PREFIX to each."
  (mapcar #'(lambda (str)
              (concat prefix ":" str))
          (split-string query)))

(defun sfs-append-queries (queries)
  "Append QUERIES to sfs-query."
  (setq sfs-query (append queries sfs-query)))

(defun sfs-author (author)
  "Add query for files by AUTHOR."
  (interactive "sEnter author: ")
  (sfs-append-queries (sfs-split-and-prefix "author" author)))

(defun sfs-containerfilename (filename)
  "Add query for files with container FILENAME."
  (interactive "sEnter container filename: ")
  (sfs-append-queries (sfs-split-and-prefix "containerfilename" filename)))

(defun sfs-date (timeframe)
  "Add query for files within TIMEFRAME."
  (interactive "sEnter timeframe")
  (sfs-append-queries (sfs-split-and-prefix "date" timeframe)))

(defun sfs-dir (dir)
  "Add query for files in DIR."
  (interactive "sEnter DIR: ")
  (sfs-append-queries (sfs-split-and-prefix "dir" dir)))

(defun sfs-ext (ext)
  "Add query for files with EXT."
  (interactive "sEnter extension(e.g. html): ")
  (sfs-append-queries (sfs-split-and-prefix "ext" ext)))

(defun sfs-filename (filename)
  "Add query for files with FILENAME."
  (interactive "sEnter filename: ")
  (sfs-append-queries (sfs-split-and-prefix "filename" filename)))

(defun sfs-keyword (keyword)
  "Add query for files containing KEYWORD."
  (interactive "sEnter keyword: ")
  (sfs-append-queries (sfs-split-and-prefix "keyword" keyword)))

(defun sfs-mime (mime)
  "Add query for files of MIME type."
  (interactive "sEnter MIME type")
  (sfs-append-queries (sfs-split-and-prefix "mime" mime)))

(defun sfs-recipient (recipient)
  "Add query for files from RECIPIENT."
  (interactive "sEnter recipient: ")
  (sfs-append-queries (sfs-split-and-prefix "recipient" recipient)))

(defun sfs-rclcat (rclcat)
  "Add query for files of RCLCAT."
  (interactive "sEnter RCLCAT type: ")
  (sfs-append-queries (sfs-split-and-prefix "rclcat" rclcat)))

(defun sfs-simple (query)
  "Add a simple QUERY."
  (interactive "sEnter query: ")
  (sfs-append-queries `(,query)))

(defun sfs-size (size)
  "Add query for files of SIZE."
  (interactive "sEnter size")
  (sfs-append-queries (sfs-split-and-prefix "size" size)))

(defun sfs-subject (subject)
  "Add query for files with SUBJECT/s."
  (interactive "sEnter subject/s: ")
  (sfs-append-queries (sfs-split-and-prefix "subject" subject)))

(provide 'sfs)
;;; sfs-tui.el ends here
