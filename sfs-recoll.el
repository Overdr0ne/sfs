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

(defvar sfs-dired-mode-map)
(setq sfs-dired-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<C-return>") 'sfs-dired-display-info)
        map))
(define-minor-mode sfs-dired-mode
  "Minor mode for sfs results in dired."
  :lighter " sfs-dired")

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

(defvar sfs--recoll-fields)
(setq sfs--recoll-fields
      '("rcludi"
        "fbytes"
        "dbytes"
        "sig"
        "url"
        "abstract"
        "filename"
        "relevancyrating"
        "mtype"
        "origcharset"
        "mtime"
        "fmtime"
        "pcbytes"
        ))
(defvar sfs-properties-highlights nil "First element for `font-lock-defaults'.")
(dolist (field sfs--recoll-fields)
  (add-to-list 'sfs-properties-highlights `(,field . font-lock-keyword-face)))

(defvar sfs-properties-mode-map)
(setq sfs-properties-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "q") 'quit-window)
        map))
(define-derived-mode sfs-properties-mode fundamental-mode "SFS Properties"
  "Major mode for SFS interactive query editor.

The query editor uses the Recoll query language, documented here:
https://www.lesbonscomptes.com/recoll/usermanual/webhelp/docs/RCL.SEARCH.LANG.html"
  ;; (setq font-lock-keywords sfs--properties-highlights)
  ;; (setq font-lock-defaults '(sfs--recoll-fields))
  (setq font-lock-defaults '(sfs-properties-highlights))
  )

(defun sfs--recoll-search-md5 (filename)
  (let (buf-str query entry)
    (setq buf-str (with-temp-buffer
                    (insert-file-contents filename)
                    (buffer-string)))
    (setq query (concat "rclmd5:"
                        (secure-hash 'md5 buf-str)))
    (car (sfs--recoll-search query))
    )
  )

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
    (car (sfs--recoll-search query))
    )
  )

(defun sfs--recoll-file-properties (filename)
  "Get recoll properties for FILENAME by looking up its md5 hash or guessing from the filename."
  (let (entry)
    (setq entry
          (-some #'(lambda (x) x)
                 `(,(sfs--recoll-search-md5 filename)
                   ,(progn
                      (message "Couldn't find entry based on it's hash, guessing the entry based on filename...")
                      (sfs--recoll-search-filename filename)))))))

(defun sfs-dired-display-info ()
  "Display a tooltip showing metadata about the file at POINT."
  (interactive)
  (let ((entry (sfs--recoll-file-properties (dired-get-filename)))
        (buffer (get-buffer-create "*SFS Properties*"))
        (filename (dired-get-filename))
        str)
    (if entry
        (progn
          (setq str (sfs--recoll-property-str entry))
          (with-current-buffer buffer
            (visual-line-mode 1)
            ;; (auto-fill-mode 1)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert str))
            (sfs-properties-mode)
            (display-buffer buffer)
            )
          )
      (message "This file has not been indexed by Recoll...")
      )))

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

(defun sfs-dired-tooltip ()
  "Display a tooltip showing metadata about the file at POINT."
  (interactive)
  (let ((entry (sfs--recoll-file-properties (dired-get-filename))))
    (popup-tip (sfs--recoll-property-str entry) :point(line-beginning-position))))

(defun sfs-dired-ivy ()
  "Operate on file metadata at POINT."
  (interactive)
  (let (entry)
    (setq entry
          (sfs--recoll-file-properties (dired-get-filename)))
    (ivy-read "Find Property: " entry
              :caller 'sfs-recoll-dired-ivy
              :action
              #'(lambda (a) (progn)
                  (kill-new (cdr a))
                  (print (concat "Entry\n---\n"
                                 (cdr a)
                                 "\n---\ncopied to kill-ring."))))))

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

(defun sfs-recoll (query-str)
  "Search QUERYSTR with recoll and display results in dired."
  (interactive "sQuery: ")
  (if (not (string= query-str ""))
      (let ((sfs-dired-buf (dired (sfs--recoll-find-urls query-str))))
        (set-buffer sfs-dired-buf)
        (rename-buffer (concat "*SFS dired* : " (buffer-name)))
        (sfs-dired-mode)
        (buffer-name))
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
