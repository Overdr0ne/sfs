;;; sfs-tui.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/Overdr0ne>
;; Maintainer:  <scmorris.dev@gmail.com>
;; Created: July 06, 2020
;; Modified: July 06, 2020
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

(require 'sfs-recoll)
(require 'dash)
(require 'general)

;;; shared stuff
(defgroup sfs nil
  "Customization group for SFS."
  :group 'Applications)

(defcustom sfs-recs `(sfs--rec-recents
                      sfs--rec-media)
  "A list of set recs to be displayed in the SFS collector TUI."
  :type  'sexp
  :group 'sfs)

(defconst sfs--researcher-buf "*SFS researcher*")
(defconst sfs--recollector-buf "*SFS recollector*")

(defun sfs--queries-or (query-list)
  "Create ORed query string from QUERY-LIST."
  (string-join query-list " OR "))

(defun sfs--split-and-prefix (prefix query)
  "Split the QUERY on space, and prepend the provided PREFIX to each."
  (mapcar #'(lambda (str)
              (concat prefix ":" str))
          (split-string query)))

(cl-defstruct (sfs-section (:constructor sfs-section-create (top bot))
                           (:copier nil))
  top bot)

(defface sfs-heading '((t (:bold t :underline t)))
  "Face used for displaying underlined bold emphasized text (_*word*_).")

(defun set-region-read-only (begin end)
  "Sets the read-only text property on the marked region.

Use `set-region-writeable' to remove this property."
  ;; See https://stackoverflow.com/questions/7410125
  ;; (interactive "r")
  (with-silent-modifications
    (put-text-property begin end 'read-only t)))

;;; Simple Recoll
(defun sfs-recoll (query-str)
  "Search QUERYSTR with recoll and display results in dired."
  (interactive "sQuery: ")
  (if (not (string= query-str ""))
      (let ((sfs-dired-buf (dired (sfs--recoll-find-urls query-str))))
        (set-buffer sfs-dired-buf)
        (rename-buffer (concat "*SFS dired* : " (buffer-name)))
        (buffer-name)
	(sfs-redir-mode))
    (message "SFS: query string empty...")))

;;; Representer
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
        "pcbytes"))
(defvar sfs-representation-highlights nil "First element for `font-lock-defaults'.")
(dolist (field sfs--recoll-fields)
  (add-to-list 'sfs-representation-highlights `(,field . font-lock-keyword-face)))

(defvar sfs-representation-mode-map)
(setq sfs-representation-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "q") 'quit-window)
        map))
(define-derived-mode sfs-represent-mode fundamental-mode "SFS:rep"
  "Major mode for SFS interactive query editor.

The query editor uses the Recoll query language, documented here:
https://www.lesbonscomptes.com/recoll/usermanual/webhelp/docs/RCL.SEARCH.LANG.html"
  (hl-line-mode)
  (setq font-lock-defaults '(sfs-representation-highlights)))

(defun sfs-represent ()
  "Display a tooltip showing metadata about the file at POINT."
  (interactive)
  (let ((entry (sfs--recoll-file-properties (dired-get-filename)))
        (buffer (get-buffer-create "*SFS represent*"))
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
	    (pop-to-buffer buffer
			   '((display-buffer-in-side-window)
			     (side . right)
			     (slot . 0)))
	    (fit-window-to-buffer)
            (sfs-represent-mode)
	    (set-window-point (get-buffer-window buffer) 0)
            ;; (display-buffer buffer)
	    ))
      (message "This file has not been indexed by Recoll..."))))

;;; Redirectory
(defvar sfs-redir-mode-map (make-keymap) "SFS redir mode keymap.")
(define-key sfs-redir-mode-map
  (kbd "<C-return>") 'sfs-represent)
(define-minor-mode sfs-redir-mode
  "Minor mode for sfs results in dired."
  nil
  " SFS:red"
  :keymap sfs-redir-mode-map)

(defun sfs-redisplay-info-hook (entry)
  (let ((buffer (get-buffer-create " *Minibuf*")) str)
    (setq str (sfs-recoll-build-tooltip entry))
    (with-current-buffer buffer
      (visual-line-mode 1)
      (auto-fill-mode 1)
      (erase-buffer)
      (insert str)
      (display-buffer buffer)))
  (select-window (get-mru-window)))

(defmacro minibuffer-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

;; (defun sfs-dired-next-line ()
;;   (interactive)
;;   "Go to next line, and display file info in minibuffer."
;;   (minibuffer-quit-and-run
;;    (dired-next-line 1)
;;    (sfs-display-info)))
;; ;; (substitute-key-definition 'dired-next-line 'sfs-dired-next-line dired-mode-map)

;; (defun sfs-dired-previous-line ()
;;   (interactive)
;;   "Go to previous line, and display file info in minibuffer."
;;   (minibuffer-quit-and-run
;;    (dired-previous-line 1)
;;    (sfs-display-info)))
;; ;; (substitute-key-definition 'dired-previous-line 'sfs-dired-previous-line dired-mode-map)

(defun sfs-redired-tooltip ()
  "Display a tooltip showing metadata about the file at POINT."
  (interactive)
  (let ((entry (sfs--recoll-file-properties (dired-get-filename))))
    (popup-tip (sfs--recoll-property-str entry) :point(line-beginning-position))))

(defun sfs-redired-ivy ()
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

;;; Researcher
(defvar sfs--query "")

(defun sfs--fields ()
  '("author"
    "containerfilename"
    "dir"
    "ext"
    "filename"
    "subject"
    "keyword"
    "recipient"
    "rclcat"
    "mime"
    "size"
    "date"))

(defun sfs--ops ()
  '("OR"
    "AND"
    ","
    "/"
    "-"))

(defun sfs--query-highlights ()
  (let (highlights)
    (dolist (field (sfs--fields))
      (push  `(,(concat field ":") . font-lock-keyword-face) highlights))
    (dolist (op (sfs--ops))
      (push  `(,op  . font-lock-function-name-face) highlights))
    highlights))

;; I need explicit function names to make help-modes useful(use which-key and edit names?), this macro automatically creates them to keep fields as single source of truth
(defmacro sfs--def-prefix-fun (field)
  "Generate a function for inserting FIELD prefix into the researcher."
  `(defun ,(intern (concat "sfs--prefix-" field)) ()
     ,(concat "Add " field ": prefix in researcher.")
     (interactive)
     (insert ,(concat field ":"))))
(dolist (field (sfs--fields))
  (eval `(sfs--def-prefix-fun ,field)))

(defvar sfs--editor-widget)

(defun sfs--id-get (id rec-tree)
  (if (and id rec-tree (listp rec-tree))
      (if (rest id)
          (sfs--id-get (rest id)
                       (alist-get (car id) rec-tree))
        (alist-get (car id) rec-tree))
    nil))

(defun sfs-id-get (id &optional rec-tree)
  (let ((path (mapcar #'intern
                      (split-string id "\\."))))
    (when (not rec-tree) (setq rec-tree sfs-recs))
    (sfs--id-get path rec-tree)))

(defun sfs--make-nested-rec (id val)
  (if id
      (if (rest id)
          `((,(car id) . ,(sfs--make-nested-rec (rest id) val)))
        `((,(car id) . ,val)))
    nil))

(defun sfs--id-add (id val rec-tree)
  (cond ((not val)
         (message "SFS: No value provided...")
         rec-tree)
        ((not id) ;; just add the leaf
         (push val rec-tree)
         rec-tree)
        ((alist-get (car id) rec-tree) ;; add updated descendents
         (setf (alist-get (car id) rec-tree)
               (sfs--id-add (rest id) val
                            (alist-get (car id) rec-tree)))
         rec-tree)
        (t ;; make branch and graft it
         (push (first (sfs--make-nested-rec id val)) rec-tree)
         rec-tree)))

(defun sfs-id-add (id val rec-tree)
  "Set sfs ID path to VAL."
  (let ((path (mapcar #'intern
                      (split-string id "\\."))))
    ;; (when (not rec-tree) (setq rec-tree sfs-recs))
    (setf rec-tree (sfs--id-add path `(,val) rec-tree)))
  rec-tree)

(defun sfs--record-query (id)
  "Save current query to ID at.some.path."
  (interactive "sEnter a path.to.an.id for query: ")
  (customize-set-variable 'sfs-recs (sfs-id-add id (string-join (split-string (widget-value sfs--editor-widget)) " ") sfs-recs))
  (quit-window))

(defun sfs--insert-rec (id)
  "Insert rec with ID at point."
  (interactive "sEnter a path.to.a.rec.id: ")
  (insert (sfs-id-get id)))

(defvar sfs-research-mode-map)
(setq sfs-research-mode-map
      (let ((map (copy-keymap widget-field-keymap)))
        (define-key map (kbd "<C-return>") 'widget-field-activate)
        ;; (define-key map (kbd "<return>") 'newline-and-indent)
        map))
(general-define-key
 :keymaps 'sfs-research-mode-map
 "C-q" '(quit-window                   :wk "quit")
 "C-r" '(sfs--record-query             :wk "record query:"))
(general-define-key
 :keymaps 'sfs-research-mode-map
 :prefix "C-c"
 "C-a" '(sfs--prefix-author            :wk "prefix author:")
 "C-c" '(sfs--prefix-containerfilename :wk "prefix containerfilename:")
 "C-d" '(sfs--prefix-dir               :wk "prefix dir:")
 "C-e" '(sfs--prefix-ext               :wk "prefix ext:")
 "C-f" '(sfs--prefix-filename          :wk "prefix filename:")
 "C-i" '(sfs--insert-rec               :wk "insert rec")
 "C-j" '(sfs--prefix-subject           :wk "prefix subject:")
 "C-k" '(sfs--prefix-keyword           :wk "prefix keyword:")
 "C-l" '(sfs--prefix-rclcat            :wk "prefix rclcat:")
 "C-m" '(sfs--prefix-mime              :wk "prefix mime:")
 "C-p" '(sfs--prefix-recipient         :wk "prefix recipient:")
 "C-t" '(sfs--prefix-date              :wk "prefix date:")
 "C-z" '(sfs--prefix-size              :wk "prefix size:"))
(define-derived-mode sfs-research-mode prog-mode "SFS:res"
  "Major mode for researching SFS recs.

The researcher uses the Recoll query language, documented here:
https://www.lesbonscomptes.com/recoll/usermanual/webhelp/docs/RCL.SEARCH.LANG.html"
  (add-hook 'post-command-hook (lambda () (fit-window-to-buffer)) 0 t)
  (setq font-lock-defaults '(sfs--query-highlights)))

(defvar sfs--logo-section)
(defun sfs-insert-logo ()
  "Make SFS logo in query builder buffer."
  ;; (widget-insert (f-read-text "./assets/graffiti.txt"))
  (let ((logo "--- S*fs : researcher ---")
        top bot)
    (setq top (point))
    (widget-insert logo)
    (setq bot (point))
    (widget-insert "\n")
    (setq sfs--logo-section (sfs-section-create top bot))))

(defun sfs--researcher-retrieve (query-str)
  "Run query through recoll and display results in a horizontal split."
  (quit-window)
  (let ((sfs-dired-buf
         (dired-noselect (sfs--recoll-find-urls query-str))))
    (when (get-buffer sfs-dired-buf)
      (pop-to-buffer sfs-dired-buf
		     '((display-buffer-same-window)))
      (set-buffer sfs-dired-buf)
      (rename-buffer (concat "*SFS dired : " (buffer-name) "*"))
      (buffer-name)
      (fit-window-to-buffer)
      (sfs-redir-mode))))

(defvar sfs--researcher-section)
(defun sfs--researcher-make-section ()
  "Make the query section in the sfs TUI."
  (let (top bot)
    (setq top (point))
    (setq sfs--editor-widget
          (widget-create 'editable-field
                         :keymap sfs-research-mode-map
                         :action (lambda (wid &rest ignore)
                                   (sfs--researcher-retrieve (widget-value wid)))
                         :notify (lambda (wid changed &rest ignore)
                                   (setq sfs--query (widget-value wid)))))
    (setq bot (point))
    (setq sfs--researcher-section (sfs-section-create top bot))
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (sfs--researcher-retrieve (widget-value sfs--editor-widget)))
                   "(C-RET)rieve")
    (widget-insert "\t")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (call-interactively #'sfs--record-query))
                   "(C-r)ecord")))

(defun sfs--researcher-make-tui ()
  "Write the tui buffer."
  (if (not (get-buffer sfs--researcher-buf))
      (let ((buffer
             (pop-to-buffer sfs--researcher-buf
			    '((display-buffer-at-bottom)
			      )
			    )))
	(with-current-buffer buffer
	  (kill-all-local-variables)
	  (remove-overlays)
	  (let ((inhibit-read-only t))
            (erase-buffer))
	  (sfs-insert-logo)
	  (sfs--researcher-make-section)
	  (fit-window-to-buffer)
	  (set-window-point (get-buffer-window sfs--researcher-buf)
                            (sfs-section-top sfs--researcher-section))
	  (widget-setup)
	  (sfs-research-mode)
	  ;; (set-region-read-only (sfs-section-top sfs--logo-section)
	  ;; 			(sfs-section-bot sfs--logo-section))
	  ))
    (progn
      (pop-to-buffer sfs--researcher-buf
		     '((display-buffer-at-bottom)))
      (sfs-research-mode)
      (fit-window-to-buffer))))

(defun sfs--tui-run (f)
  "Call F in TUI context."
  (call-interactively f)
  (sfs--researcher-make-tui))

(defun sfs-research ()
  "Open the SFS researcher to recompose and record recollections."
  (interactive)
  (sfs--researcher-make-tui))

;;; Recollect
(defun sfs--rec-recents ()
  "Make the rec for recently touched files."
  (let ((recent-recollections '())
        (recents
         `((day . ,(calendar-current-date))
           (week . ,(calendar-gregorian-from-absolute
                     (+ (calendar-absolute-from-gregorian (calendar-current-date)) -7)))
           (month . ,(calendar-gregorian-from-absolute
                      (+ (calendar-absolute-from-gregorian (calendar-current-date)) -30)))
           (year . ,(calendar-gregorian-from-absolute
                     (+ (calendar-absolute-from-gregorian (calendar-current-date)) -365)))))
        query-str day month year rtn-date)
    (dolist (elmt recents)
      (setq rtn-date (cdr elmt))
      (setq day (nth 1 rtn-date))
      (setq month (nth 0 rtn-date))
      (setq year (nth 2 rtn-date))
      (setq query-str (format "date:%d-%d-%d/" year month day))
      (push `(,(car elmt) . (,query-str)) recent-recollections))

    `(recents . ,recent-recollections)))

(defun sfs--rec-media ()
  "Make the media rec."
  `(media .
          ((comics . (,(sfs--queries-or (sfs--split-and-prefix "ext" "djvu cbr cbz cb7 cbt cba"))))
           (books . (,(sfs--queries-or (sfs--split-and-prefix "ext" "ibooks pdf epub pbd djvu azw azw3 kf8 kfx fb2 mobi opf"))))
           (documents . (,(sfs--queries-or (sfs--split-and-prefix "ext" "pdf doc docx txt tex"))))
           (music . (,(sfs--queries-or (sfs--split-and-prefix "ext" "mp3 aac"))
                     (lossless ,(sfs--queries-or (sfs--split-and-prefix "ext" "wav flac aiff")))
                     (lossy ,(sfs--queries-or (sfs--split-and-prefix "ext" "mp3 aac")))))
           (video . ,(sfs--split-and-prefix "ext" "mp4 mov wmv flv avi webm mkv"))
           (text . ("mime:text")))))

(defun sfs--insert-heading (heading depth)
  "SFS insert heading at depth."
  (dotimes (n depth)
    (insert "*"))
  (insert (concat " " heading "\n")))

(defun sfs--insert-rec-section (rec depth)
  "Make the section for REC with LABEL in the sfs rec TUI."
  (if rec
      (cond
       ((listp rec)
        (sfs--insert-rec-section (first rec) (+ depth 1))
        (sfs--insert-rec-section (rest rec) depth))
       ((symbolp rec)
        (sfs--insert-heading (concat "rec:" (symbol-name rec)) depth))
       (t (sfs--insert-heading rec (+ depth 1))))
    nil))

(defvar sfs-recollect-mode-map)
(setq sfs-recollect-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<S-return>") 'sfs-fetch-recollection-at-point)
        (define-key map (kbd "C-q") 'kill-this-buffer)
        map))
(general-define-key
 :keymaps 'sfs-recollect-mode-map
 :prefix "C-c"
 "C-a" '(sfs--prefix-author            :wk "prefix author:")
 "C-c" '(sfs--prefix-containerfilename :wk "prefix containerfilename:")
 "C-d" '(sfs--prefix-dir               :wk "prefix dir:")
 "C-e" '(sfs--prefix-ext               :wk "prefix ext:")
 "C-f" '(sfs--prefix-filename          :wk "prefix filename:")
 "C-i" '(sfs--insert-rec               :wk "insert rec")
 "C-j" '(sfs--prefix-subject           :wk "prefix subject:")
 "C-k" '(sfs--prefix-keyword           :wk "prefix keyword:")
 "C-l" '(sfs--prefix-rclcat            :wk "prefix rclcat:")
 "C-m" '(sfs--prefix-mime              :wk "prefix mime:")
 "C-p" '(sfs--prefix-recipient         :wk "prefix recipient:")
 "C-t" '(sfs--prefix-date              :wk "prefix date:")
 "C-z" '(sfs--prefix-size              :wk "prefix size:"))
(define-derived-mode sfs-recollect-mode org-mode "SFS:rec"
  "Mode for SFS interactive recollections interface."
  (hide-sublevels 2)
  (setq-local fit-window-to-buffer-horizontally t)
  ;; (setq-local window-max-width 35)
  (add-hook 'post-command-hook
	    (lambda () (fit-window-to-buffer nil 999 0 35))
	    0 t)
  (visual-line-mode)
  (fit-window-to-buffer nil 999 0 35))

(defun sfs--recollector-make-tui ()
  "Write the tui buffer."
  (pop-to-buffer sfs--recollector-buf
		 '((display-buffer-in-side-window)
		   (side . left)
		   (slot . 0)))
  (with-current-buffer sfs--recollector-buf
    (erase-buffer)
    ;; (push-mark)
    (dolist (el sfs-recs)
      (cond ((listp el)
             (sfs--insert-rec-section el 0))
            ((symbol-function el)
             (sfs--insert-rec-section (funcall el) 0))
            (t (sfs--insert-rec-section (symbol-value el) 0))))
    ;; (set-region-read-only (mark) (point))
    ;;TODO tags section
    (insert "* rec:custom")
    ;; (push-mark)
    (insert "\n")
    (set-window-point (get-buffer-window sfs--recollector-buf)
                      0)
    (sfs-recollect-mode)))

(defun sfs--heading-depth (heading)
  (if (string-equal (substring heading 0 1) "*")
      (+ 1 (sfs--heading-depth (substring heading 1)))
    0))

(defun sfs--headings-first-subtree-list (headings depth)
  (if headings
      (if (> (sfs--heading-depth (first headings))
             depth)
          (cons (first headings)
                (sfs--headings-first-subtree-list (rest headings)
                                                  depth))
        nil)
    nil))
(defun sfs--headings-first-tree-list (headings)
  (if headings
      (cons (first headings)
            (sfs--headings-first-subtree-list (rest headings)
                                              (sfs--heading-depth (first headings))))
    nil))

(defun sfs--headings-rest-list- (headings depth)
  (if headings
      (if (= depth (sfs--heading-depth (first headings)))
          headings
        (sfs--headings-rest-list- (rest headings) depth))
    headings))
(defun sfs--headings-rest-list (headings)
  (if headings
      (sfs--headings-rest-list- (rest headings)
                                (sfs--heading-depth (first headings)))
    nil))

(defun sfs--get-title (heading)
  (string-join (rest (split-string heading " "))
               " "))

(defun sfs--headings-to-tree (headings)
  (if headings
      ;; cons the first query tree onto the remaining query trees
      (cons (cons (sfs--get-title (first headings))
                  (sfs--headings-to-tree (rest (sfs--headings-first-tree-list headings))))
            (sfs--headings-to-tree (sfs--headings-rest-list headings)))
    nil))

(defun sfs--heading-at-point-to-tree ()
  (let (headings tree)
    (setq headings (buffer-substring-no-properties (point) (point-max)))
    (setq headings (split-string headings "\n"))
    (setq headings (remove-if (lambda (el) (string= el "")) headings))
    (first (sfs--headings-to-tree headings))))

(defun sfs--tree-to-query (tree)
  (let ((root (first tree)))
    (cond ((or (string= root "AND")
               (string= root "OR"))
           (concat "("
                   (string-join (mapcar #'sfs--tree-to-query (rest tree))
                                (concat " " root " "))
                   ")"))
          ((string= (first (split-string root ":"))
                    "rec")
           (concat "("
                   (string-join (mapcar #'sfs--tree-to-query (rest tree))
                                " OR ")
                   ")"))
          (t root))))

(defun sfs-recollect-at-point ()
  "Fetch the recollection under the heading at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((buf (dired-noselect
		(sfs--recoll-find-urls
		 (sfs--tree-to-query
		  (sfs--heading-at-point-to-tree))))))
      (quit-window)
      (pop-to-buffer buf
		     '((display-buffer-same-window)))
      (sfs-redir-mode))))

(defun sfs-record-recollection-at-point (id)
  "Save the recollection at point to rec ID."
  (interactive "sEnter id path: ")
  (let ((query-str (sfs-recollect-at-point)))
    (if (alist-get id sfs-recs)
        (setf (alist-get (intern id) sfs-recs)
              `(,query-str))
      (push `(,(intern id) ,(widget-value sfs--editor-widget)) sfs-recs))))

(defun sfs-recollect ()
  "Access SFS recollections in an interactive TUI."
  (interactive)
  (sfs--recollector-make-tui))

(provide 'sfs-tui)
;;; sfs-tui.el ends here
