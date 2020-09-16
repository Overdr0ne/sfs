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
(require 'dash)

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

(defface sfs-heading '((t (:bold t :underline t)))
  "Face used for displaying underlined bold emphasized text (_*word*_)."
  )

(defun sfs--insert-section-heading (heading)
  (put-text-property 0 (length heading) 'face 'sfs-heading
                     heading)
  (widget-insert heading ":\n")
  )

(defun sfs-insert-logo ()
  "Make SFS logo in search buffer."
  (widget-insert (f-read-text "./assets/graffiti.txt"))
  )

(defstruct (sfs-field (:constructor sfs-field-create (label collector desc prefix))
                      (:copier nil))
  label collector desc prefix)

(defvar sfs-fields nil)
(setq sfs-fields
      `( ,(sfs-field-create "(a)uthor" #'sfs-author "looks for AUTHOR in Recoll's extracted metadata" "author")
         ,(sfs-field-create "(c)ontainer" #'sfs-containerfilename "filename set for both top-level and subdocuments" "containerfilename")
         ,(sfs-field-create "(d)irectory" #'sfs-dir "full or relative, tildes and wildcards expanded, positive or negative, e.g. bin/ -~/" "dir")
         ,(sfs-field-create "(e)xtension" #'sfs-ext "filename extension" "ext")
         ,(sfs-field-create "(f)ilename" #'sfs-filename "not necessarily set for compound documents, e.g. those in an EPUB section" "filename")
         ,(sfs-field-create "s(i)mple" #'sfs-simple "generic google-like search query" "")
         ,(sfs-field-create "sub(j)ect" #'sfs-subject "subject/title/caption of the document from extracted metadata" "subject")
         ,(sfs-field-create "(k)eyword" #'sfs-keyword "document-specified keywords(few documents actually have any)" "keyword")
         ,(sfs-field-create "(r)ecipient" #'sfs-recipient "looks for RECIPIENT in Recoll's extracted metadata" "recipient")
         ,(sfs-field-create "rc(l)cat" #'sfs-rclcat "e.g. text or -presentation etc." "rclcat")
         ,(sfs-field-create "(m)ime type" #'sfs-mime "e.g. text/plain or text/* etc." "mime")
         ,(sfs-field-create "(s)ize" #'sfs-size "e.g. >1000k or <30G or =10g etc." "size")
         ,(sfs-field-create "da(t)e" #'sfs-date "YYYY-MM-DD/YYYY-MM-DD or YYYY/ or /YYYY-MM or YYYY etc." "date")
         ))

(defun get-field-by-prefix (prefix)
  (-filter (lambda (field)
             (string= (sfs-field-prefix field) prefix))
           sfs-fields))

(defun sfs-make-section-fields ()
  "Make the fields section in the search buffer."
  (let ()
    (sfs--insert-section-heading "Fields")
    (push-mark (point))
    (mapcar
     (lambda (field)
       (widget-create 'push-button
                      :notify (lambda (&rest ignore)
                                (sfs--tui-run (sfs-field-collector field))
                                )
                      (sfs-field-label field)
                      )
       (widget-insert (concat "\t: " (sfs-field-desc field) "\n"))
       )
     sfs-fields)
    (let ((inhibit-read-only t))
      ;; (pop-mark)
      (align-regexp (mark) (point) "\\(\\s-*\\):"))
    )
  )

(defvar sfs-query nil)

(defun sfs-make-section-query ()
  "Make the query section in the sfs TUI."
  (sfs--insert-section-heading "Query")
  (dolist (elmt sfs-query)
    (widget-insert (concat elmt "\n")))
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (sfs-ex-query))
                 "e(x)ecute") )

(defun sfs--make-search-tui ()
  "Write the tui buffer."
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

(defun sfs--tui-run (f)
  "Call F in TUI context."
  (call-interactively f)
  (sfs--make-search-tui))

(defun sfs ()
  "Go to SFS start page."
  (interactive)
  (sfs--make-search-tui)
  )

(defun sfs-save-query (id)
  "Save current query to ID."
  (interactive "sEnter ID for query: ")
  (add-to-list 'sfs-favs `(,id . (,sfs-query))))

(defun sfs-ex-query ()
  "Execute search query."
  (interactive)
  (sfs-recoll (string-join sfs-query " ")))

(defun sfs-split-and-prefix (prefix query)
  "Split the QUERY on space, and prepend the provided PREFIX to each."
  (mapcar #'(lambda (str)
              (concat prefix ":" str))
          (split-string query)))

(defun sfs-query-append (input-str prefix)
  "Append INPUT-STR to sfs-query with PREFIX."
  ;; (interactive (concat "sEnter " prefix ": "))
  (setq sfs-query (append
                   (sfs-split-and-prefix prefix input-str)
                   sfs-query))
  (sfs--make-search-tui))

(defun sfs-author (author)
  "Add query for files by AUTHOR."
  (interactive "sEnter author: ")
  (sfs-query-append author "author"))

(defun sfs-containerfilename (filename)
  "Add query for files with container FILENAME."
  (interactive "sEnter container filename/s: ")
  (sfs-query-append filename "containerfilename"))

(defun sfs-date (timeframe)
  "Add query for files within TIMEFRAME."
  (interactive "sEnter timeframe/s: ")
  (sfs-query-append timeframe "date"))

(defun sfs-dir (dir)
  "Add query for files in DIR."
  (interactive "sEnter directory/s: ")
  (sfs-query-append dir "dir"))

(defun sfs-ext (ext)
  "Add query for files with EXT."
  (interactive "sEnter file extension/s: ")
  (sfs-query-append ext "ext"))

(defun sfs-filename (filename)
  "Add query for files with FILENAME."
  (interactive "sEnter filename/s: ")
  (sfs-query-append filename "filename"))

(defun sfs-keyword (keyword)
  "Add query for files containing KEYWORD."
  (interactive "sEnter keyword/s: ")
  (sfs-query-append keyword "keyword"))

(defun sfs-mime (mime)
  "Add query for files of MIME type."
  (interactive "sEnter MIME type/s: ")
  (sfs-query-append mime "mime"))

(defun sfs-recipient (recipient)
  "Add query for files from RECIPIENT."
  (interactive "sEnter recipient: ")
  (sfs-query-append recipient "recipient"))

(defun sfs-rclcat (rclcat)
  "Add query for files of RCLCAT."
  (interactive "sEnter RCLCAT type/s: ")
  (sfs-query-append rclcat "rclcat"))

(defun sfs-simple (query)
  "Add a simple QUERY."
  (interactive "sEnter query: ")
  (sfs-query-append `(,query) ""))

(defun sfs-size (size)
  "Add query for files of SIZE."
  (interactive "sEnter size/s: ")
  (sfs-query-append size "size"))

(defun sfs-subject (subject)
  "Add query for files with SUBJECT/s."
  (interactive "sEnter subject/s: ")
  (sfs-query-append subject "subject"))

(provide 'sfs-tui)
;;; sfs-tui.el ends here
