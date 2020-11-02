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

(defcustom sfs-class-favs nil
  "An alist to save your favorite SFS queries, associating queries with symbols. These are then used to generate the collector menu, or can be recalled in the SFS query editor."
  :group 'sfs)

(defun sfs--queries-or (query-list)
  "Create ORed query string from QUERY-LIST."
  (string-join query-list " OR "))

(defun sfs-split-and-prefix (prefix query)
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

(defun sfs--insert-section-heading (heading)
  "Format and insert HEADING into file."
  (put-text-property 0 (length heading) 'face 'sfs-heading
                     heading)
  (insert heading ":\n"))

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

;;; Query Editor

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
  "Generate a function for inserting FIELD prefix into the query editor."
  `(defun ,(intern (concat "sfs--prefix-" field)) ()
     ,(concat "Add " field ": prefix in query editor.")
     (interactive)
     (insert ,(concat field ":"))))
(dolist (field (sfs--fields))
  (eval `(sfs--def-prefix-fun ,field)))

(defvar sfs--editor-widget)
(defun sfs-save-query (id)
  "Save current query to ID under the sfs-class-favs custom variable."
  (interactive "sEnter ID for query: ")
  (push `(,id . ,(widget-value sfs--editor-widget)) sfs-class-favs)
  sfs-class-favs)

(defun sfs-query-editor-insert-set (set-id)
  "Insert the given SET-ID into the query editor."
  (interactive "sEnter a set: ")
  (widget-insert (cdr (alist-get set-id (sfs--class-all)))))

(defvar sfs-query-editor-mode-map)
(setq sfs-query-editor-mode-map
      (let ((map (copy-keymap widget-field-keymap)))
        (define-key map (kbd "<C-return>") 'widget-field-activate)
        (define-key map (kbd "<return>") 'newline-and-indent)
        map))
(general-define-key
 :keymaps 'sfs-query-editor-mode-map
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
 "C-o" '(sfs--insert-collection        :wk "insert collection")
 "C-q" '(quit-window                   :wk "quit window")
 "C-s" '(sfs--prefix-size              :wk "prefix size:")
 "C-t" '(sfs--prefix-date              :wk "prefix date:")
 "C-v" '(sfs-save-query                :wk "save query"))


(define-derived-mode sfs-query-editor-mode prog-mode "SFS"
  "Major mode for SFS interactive query editor.

The query editor uses the Recoll query language, documented here:
https://www.lesbonscomptes.com/recoll/usermanual/webhelp/docs/RCL.SEARCH.LANG.html"
  (setq font-lock-defaults '(sfs--query-highlights)))


(define-widget 'sfs-query 'default
  "An editable text field for sfs queries.
Note: In an `sfs-query' widget, the `%v' escape must be preceded
by some other text in the `:format' string (if specified)."
  :convert-widget 'widget-value-convert-widget
  :keymap sfs-query-editor-mode-map
  :format "%v"
  :help-echo "M-TAB: complete field; RET: enter value"
  :value ""
  :prompt-internal 'widget-field-prompt-internal
  :prompt-history 'widget-field-history
  :prompt-value 'widget-field-prompt-value
  :action 'widget-field-action
  :validate 'widget-field-validate
  :valid-regexp ""
  :error "Field's value doesn't match allowed forms"
  :value-create 'widget-field-value-create
  :value-set 'widget-field-value-set
  :value-delete 'widget-field-value-delete
  :value-get 'widget-field-value-get
  :match 'widget-field-match)

(defvar sfs--logo-section)
(defun sfs-insert-logo ()
  "Make SFS logo in query builder buffer."
  ;; (widget-insert (f-read-text "./assets/graffiti.txt"))
  (let ((logo "--- S*fs : query editor ---")
        top bot)
    (setq top (point))
    (widget-insert logo)
    (setq bot (point))
    (widget-insert "\n")
    (setq sfs--logo-section (sfs-section-create top bot))))



(defvar sfs--query-editor-section)
(defun sfs-make-section-query-editor ()
  "Make the query section in the sfs TUI."
  (let (top bot)
    (setq top (point))
    (setq sfs--editor-widget
          (widget-create 'sfs-query
                         :keymap sfs-query-editor-mode-map
                         :action (lambda (wid &rest ignore)
                                   (sfs-recoll (widget-value wid)))
                         :notify (lambda (wid changed &rest ingore)
                                   (setq sfs--query (widget-value wid)))))
    (setq bot (point))
    (setq sfs--query-editor-section (sfs-section-create top bot))
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (sfs-recoll (widget-value sfs--editor-widget)))
                   "Execute")
    (widget-insert "(C-RET)\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (call-interactively #'sfs-save-query))
                   "Save query"))
  (widget-insert "(C-c C-v)"))


(defun sfs--make-query-editor-tui ()
  "Write the tui buffer."
  (let ((buffer
         (switch-to-buffer "*SFS*")))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (remove-overlays)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (sfs-insert-logo)
      (sfs-make-section-query-editor)
      (set-window-point (get-buffer-window "*SFS*")
                        (sfs-section-top sfs--query-editor-section))
      (widget-setup)
      (sfs-query-editor-mode)
      (set-region-read-only (sfs-section-top sfs--logo-section)
                            (sfs-section-bot sfs--logo-section)))))


(defun sfs--tui-run (f)
  "Call F in TUI context."
  (call-interactively f)
  (sfs--make-query-editor-tui))

(defun sfs ()
  "Go to SFS start page."
  (interactive)
  (sfs--make-query-editor-tui))


;;; Collections TUI
(defun sfs--class-recents ()
  "Make the class for recently touched files."
  (let ((recent-collections '())
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
      (add-to-list 'recent-collections `(,(car elmt) . ,query-str)))

    recent-collections))

(defun sfs--class-places ()
  "Make the places class."
  `((comics . ,(sfs--queries-or (sfs-split-and-prefix "ext" "djvu cbr cbz cb7 cbt cba")))
    (books . ,(sfs--queries-or (sfs-split-and-prefix "ext" "ibooks pdf epub pbd djvu azw azw3 kf8 kfx fb2 mobi opf")))
    (documents . ,(sfs--queries-or (sfs-split-and-prefix "ext" "pdf doc docx txt tex")))
    (music . "mime:audio")
    (video . "mime:video")
    (text . "mime:text")))

(defun sfs--class-all ()
  `(,@(sfs--class-places)
    ,@sfs-class-favs
    ,@(sfs--class-recents)))

(defun sfs--insert-class-section (class id)
  "Make the section for CLASS with LABEL in the sfs class TUI."
  ;; (sfs--insert-section-label heading)
  (insert "* class:" id "\n")
  (dolist (elmt class)
    (insert (concat "** set:" (symbol-name (car elmt)) "\n"))))

(defun sfs-insert-set (set-id)
  "Insert the set associated with SET-ID at point."
  (interactive "sEnter an sfs set: ")
  (insert (alist-get set-id (sfs--class-all))))

(defvar sfs-collections-mode-map)
(setq sfs-collections-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<C-return>") 'sfs-fetch-collection-at-point)
        map))
(define-derived-mode sfs-collections-mode outline-mode "SFS Collections"
  "Mode for SFS interactive collections interface.")
  ;; (outline-mode)


(defface sfs--read-only '((default . (:background "beige")))
  "Face for `my-read-only-region'")

(defun sfs--read-only-region (begin end)
  "Make the marked region read-only.  See also `my-writeable-region'.

Read-only text is given the face `sfs--read-only'."
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (add-text-properties begin end '(read-only t))
      (let ((overlay (make-overlay begin end)))
        (overlay-put overlay 'sfs--type 'read-only)
        (overlay-put overlay 'face 'sfs--read-only)))))

(defun sfs--writeable-region (begin end)
  "Make the marked region writeable.  See also `my-read-only-region'."
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (remove-text-properties begin end '(read-only t))
      (remove-overlays begin end 'sfs--type 'read-only))))

(defcustom sfs-collector-classes '((sfs-class-favs     "favorites")
                                   (sfs--class-recents "recents")
                                   (sfs--class-places  "places"))
  "A list of set classes to be displayed in the SFS collector TUI."
  :type  'sexp
  :group 'sfs)

(defun sfs--make-collections-tui ()
  "Write the tui buffer."
  (let ((buffer (switch-to-buffer "*SFS Collections*")))
    (with-current-buffer buffer
      (erase-buffer)
      (push-mark)
      ;;TODO tags section
      (dolist (el sfs-collector-classes)
        (if (symbol-function (first el))
            (sfs--insert-class-section (funcall (first el)) (second el))
          (sfs--insert-class-section (symbol-value (first el)) (second el))))
      (sfs--insert-class-section nil "scratch")
      (push-mark)
      (insert "\n")
      ;; (sfs--read-only-region (mark)
      ;;                        (progn (pop-mark)
      ;;                               (mark)))
      (sfs-collections-mode))))

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
                    "set")
           (alist-get (intern (second (split-string root ":")))
                      (sfs--class-all)))
          ;; also let people search entire classes as the union of its sets
          ((string= (first (split-string root ":"))
                    "class")
           (concat "("
                   (string-join (mapcar #'sfs--tree-to-query (rest tree))
                                " OR ")
                   ")"))
          (t root))))

(defun sfs-fetch-collection-at-point ()
  "Fetch the collection under the heading at point."
  (interactive)
  (sfs-recoll (sfs--tree-to-query (sfs--heading-at-point-to-tree))))

(defun sfs-collections ()
  "Go to SFS start page."
  (interactive)
  (sfs--make-collections-tui))


(provide 'sfs-tui)
;;; sfs-tui.el ends here
