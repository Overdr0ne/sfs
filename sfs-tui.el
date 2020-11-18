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
(require 'wid-edit)
(require 'general)

;;; shared stuff
(defgroup sfs nil
  "Customization group for SFS."
  :group 'Applications)

(defcustom sfs-favorites nil
  "An alist to save your favorite SFS queries, associating queries with symbols. These are then used to generate the collector menu, or can be recalled in the recollector."
  :group 'sfs)
(setq sfs-favorites nil)

(defcustom sfs-recs `((favorites . ,sfs-favorites)
                      sfs--rec-recents
                      sfs--rec-media)
  "A list of set recs to be displayed in the SFS collector TUI."
  :type  'sexp
  :group 'sfs)
(setq sfs-recs `((favorites . ,sfs-favorites)
                 sfs--rec-recents
                 sfs--rec-media))

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


;; (defun sfs-ex-query ()
;;   "Execute query builder query."
;;   (interactive)
;;   (let (sep)
;;     (sfs-recoll (string-join (cdr sfs--query ())))))

(defface sfs-logo
  '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "Purple")
    (((class color) (min-colors 88) (background dark))  :foreground "Cyan1")
    (((class color) (min-colors 16) (background light)) :foreground "Purple")
    (((class color) (min-colors 16) (background dark))  :foreground "Cyan")
    (((class color) (min-colors 8)) :foreground "cyan" :weight bold)
    (t (:bold t)))
  "Face used for displaying underlined bold emphasized text (_*word*_).")

(defun set-region-read-only (begin end)
  "Sets the read-only text property on the marked region.

Use `set-region-writeable' to remove this property."
  ;; See https://stackoverflow.com/questions/7410125
  (interactive "r")
  (with-silent-modifications
    (put-text-property begin end 'read-only t)))

;;; Recollector

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
  "Generate a function for inserting FIELD prefix into the recollector."
  `(defun ,(intern (concat "sfs--prefix-" field)) ()
     ,(concat "Add " field ": prefix in recollector")
     (interactive)
     (insert ,(concat field ":"))))
(dolist (field (sfs--fields))
  (eval `(sfs--def-prefix-fun ,field)))

(defvar sfs--editor-widget)
(defun sfs-add-favorite (id)
  "Save current query to ID under the sfs-favorites custom variable."
  (interactive "sEnter ID for query: ")
  (setf sfs-favorites (sfs-id-add id (widget-value sfs--editor-widget) sfs-favorites))
  sfs-favorites)

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

(defun sfs-save-query (id)
  "Save current query to ID under the sfs-favorites custom variable."
  (interactive "sEnter a path.to.an.id for query: ")
  (setf sfs-recs (sfs-id-add id (widget-value sfs--editor-widget) sfs-recs)))

(defvar sfs-recollector-mode-map)
(setq sfs-recollector-mode-map
      (let ((map (copy-keymap widget-field-keymap)))
        (define-key map (kbd "<C-return>") 'widget-field-activate)
        (define-key map (kbd "<return>") 'newline-and-indent)
        map))
(general-define-key
 :keymaps 'sfs-recollector-mode-map
 :prefix "C-c"
 "C-a" '(sfs--prefix-author            :wk "prefix author:")
 "C-c" '(sfs--prefix-containerfilename :wk "prefix containerfilename:")
 "C-d" '(sfs--prefix-dir               :wk "prefix dir:")
 "C-e" '(sfs--prefix-ext               :wk "prefix ext:")
 "C-f" '(sfs--prefix-filename          :wk "prefix filename:")
 "C-j" '(sfs--prefix-subject           :wk "prefix subject:")
 "C-k" '(sfs--prefix-keyword           :wk "prefix keyword:")
 "C-r" '(sfs--prefix-recipient         :wk "prefix recipient:")
 "C-l" '(sfs--prefix-rclcat            :wk "prefix rclcat:")
 "C-m" '(sfs--prefix-mime              :wk "prefix mime:")
 "C-o" '(sfs--insert-recollection      :wk "insert collection")
 "C-q" '(quit-window                   :wk "quit window")
 "C-s" '(sfs-save-query                :wk "prefix size:")
 "C-t" '(sfs--prefix-date              :wk "prefix date:")
 "C-v" '(sfs-add-favorite              :wk "save query")
 "C-z" '(sfs--prefix-size              :wk "prefix size:"))
(define-derived-mode sfs-recollector-mode prog-mode "SFS"
  "Major mode for SFS interactive recollector.

The recollector uses the Recoll query language, documented here:
https://www.lesbonscomptes.com/recoll/usermanual/webhelp/docs/RCL.SEARCH.LANG.html"
  (setq font-lock-defaults '(sfs--query-highlights)))

(defvar sfs--logo-section)
(defun sfs-insert-logo ()
  "Make SFS logo in query builder buffer."
  ;; (widget-insert (f-read-text "./assets/graffiti.txt"))
  (let ((logo "--- S*fs : recollector ---")
        top bot)
    (setq top (point))
    (widget-insert logo)
    (setq bot (point))
    (widget-insert "\n")
    (setq sfs--logo-section (sfs-section-create top bot))))


(defvar sfs--recollector-section)
(defun sfs-make-section-recollector ()
  "Make the query section in the sfs TUI."
  (let (top bot)
    (setq top (point))
    (setq sfs--editor-widget
          (widget-create 'editable-field
                         :keymap sfs-recollector-mode-map
                         :action (lambda (wid &rest ignore)
                                   (sfs-recoll (widget-value wid)))
                         :notify (lambda (wid changed &rest ignore)
                                   (setq sfs--query (widget-value wid)))))
    (setq bot (point))
    (setq sfs--recollector-section (sfs-section-create top bot))
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (sfs-recoll (widget-value sfs--editor-widget)))
                   "Execute")
    (widget-insert "(C-RET)\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (call-interactively #'sfs-add-favorite))
                   "Favorite")
    (widget-insert "(C-c C-v)\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (call-interactively #'sfs-save-query))
                   "Save")
    (widget-insert "(C-c C-s)")))

(defun sfs--make-recollector-tui ()
  "Write the tui buffer."
  (let ((buffer
         (switch-to-buffer "*SFS: recollector*")))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (remove-overlays)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (sfs-insert-logo)
      (sfs-make-section-recollector)
      (set-window-point (get-buffer-window "*SFS: recollector*")
                        (sfs-section-top sfs--recollector-section))
      (widget-setup)
      (sfs-recollector-mode)
      (set-region-read-only (sfs-section-top sfs--logo-section)
                            (sfs-section-bot sfs--logo-section)))))

(defun sfs--tui-run (f)
  "Call F in TUI context."
  (call-interactively f)
  (sfs--make-recollector-tui))

(defun sfs-recollect ()
  "Open the SFS recollector to interactively compose Recoll queries."
  (interactive)
  (sfs--make-recollector-tui))

;;; Recollections TUI
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
           (music . ("mime:audio"
                     ,(sfs--queries-or (sfs--split-and-prefix "ext" "mp3 aac"))
                     (lossless ,(sfs--queries-or (sfs--split-and-prefix "ext" "wav flac aiff")))
                     (lossy ,(sfs--queries-or (sfs--split-and-prefix "ext" "mp3 aac")))))
           (video . ("mime:video"))
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

(defvar sfs-recollections-mode-map)
(setq sfs-recollections-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<return>") 'sfs-fetch-recollection-at-point)
        map))
(define-derived-mode sfs-recollections-mode outline-mode "SFS Recollections"
  "Mode for SFS interactive recollections interface.")

(defun sfs--make-recollections-tui ()
  "Write the tui buffer."
  (let ((buffer (switch-to-buffer "*SFS Recollections*")))
    (setf (alist-get 'favorites sfs-recs)
          sfs-favorites)
    (with-current-buffer buffer
      (erase-buffer)
      (push-mark)
      (dolist (el sfs-recs)
        (cond ((listp el)
               (sfs--insert-rec-section el 0))
              ((symbol-function el)
               (sfs--insert-rec-section (funcall el) 0))
              (t (sfs--insert-rec-section (symbol-value el) 0))))
      ;;TODO tags section
      (insert "* rec:custom")
      (push-mark)
      (insert "\n")
      (sfs-recollections-mode))))

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

(defun sfs-fetch-recollection-at-point ()
  "Fetch the recollection under the heading at point."
  (interactive)
  (sfs-recoll (sfs--tree-to-query (sfs--heading-at-point-to-tree))))

(defun sfs-save-recollection-at-point (id)
  "Save the recollection at point to rec ID."
  (interactive "sEnter id path: ")
  (let ((query-str (sfs-fetch-recollection-at-point)))
    (if (alist-get id sfs-recs)
        (setf (alist-get (intern id) sfs-recs)
              `(,query-str))
      (push `(,(intern id) ,(widget-value sfs--editor-widget)) sfs-favorites))))

(defun sfs-recollections ()
  "Open the SFS recollections TUI to interact with SFS recollections."
  "Go to SFS start page."
  (interactive)
  (sfs--make-recollections-tui))

(provide 'sfs-tui)
;;; sfs-tui.el ends here
