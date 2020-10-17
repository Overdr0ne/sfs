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
(require 'wid-edit)
(require 'general)

;;; shared stuff
(defun sfs--queries-or (query-list)
  "Create ORed query string from QUERY-LIST."
  (string-join query-list " OR "))

(defun sfs-split-and-prefix (prefix query)
  "Split the QUERY on space, and prepend the provided PREFIX to each."
  (mapcar #'(lambda (str)
              (concat prefix ":" str))
          (split-string query)))

(defvar sfs--collection-favs '())

(defun sfs--collection-make-timespans ()
  "Make the section in the sfs collections TUI for recently touched files."
  (let ((timespan-collections '())
        (timespans
         `(("day" . ,(calendar-current-date))
           ("week" . ,(calendar-gregorian-from-absolute
                       (+ (calendar-absolute-from-gregorian (calendar-current-date)) -7)))
           ("month" . ,(calendar-gregorian-from-absolute
                        (+ (calendar-absolute-from-gregorian (calendar-current-date)) -30))) ) )
        query-str day month year rtn-date)
    (dolist (elmt timespans)
      (setq rtn-date (cdr elmt))
      (setq day (nth 1 rtn-date))
      (setq month (nth 0 rtn-date))
      (setq year (nth 2 rtn-date))
      (setq query-str (format "date:%d-%d-%d" year month day))
      (add-to-list 'timespan-collections `(,(car elmt) . ,query-str))
      )
    timespan-collections))
(defvar sfs--collection-timespans)
(setq sfs--collection-timespans (sfs--collection-make-timespans))

(defun sfs--collections-make-places ()
  "Make the places collections."
  `(("comics" . ,(sfs--queries-or (sfs-split-and-prefix "ext" "djvu cbr cbz cb7 cbt cba")))
    ("books" . ,(sfs--queries-or (sfs-split-and-prefix "ext" "ibooks pdf epub pbd djvu azw azw3 kf8 kfx fb2 mobi opf")))
    ("documents" . ,(sfs--queries-or (sfs-split-and-prefix "ext" "pdf doc docx txt tex")))
    ("music" . "mime:audio")
    ("video" . "mime:video")
    ("text" . "mime:text")))
(defvar sfs--collection-places)
(setq sfs--collection-places (sfs--collections-make-places))

(defvar sfs--collection-all)
(setq sfs--collection-all `(,@sfs--collection-places
                            ,@sfs--collection-favs
                            ,@sfs--collection-timespans))

(cl-defstruct (sfs-section (:constructor sfs-section-create (top bot))
                           (:copier nil))
  top bot)

(defface sfs-heading '((t (:bold t :underline t)))
  "Face used for displaying underlined bold emphasized text (_*word*_)."
  )

(defun sfs-ex-query ()
  "Execute query builder query."
  (interactive)
  (let (sep)
    (sfs-recoll (string-join (cdr sfs--query ())))))

(defun sfs--insert-section-heading (heading)
  "Format and insert HEADING into file."
  (put-text-property 0 (length heading) 'face 'sfs-heading
                     heading)
  (widget-insert heading ":\n"))

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

(defvar sfs--fields)
(setq sfs--fields
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
        "date"
        ))

(defvar sfs--ops)
(setq sfs--ops
      '("OR"
        "AND"
        ","
        "/"
        "-"))

(defvar sfs-query-highlights nil "First element for `font-lock-defaults'.")
(dolist (field sfs--fields)
  (add-to-list 'sfs-query-highlights `(,(concat field ":") . font-lock-keyword-face)))
(dolist (op sfs--ops)
  (add-to-list 'sfs-query-highlights `(,op  . font-lock-function-name-face)))

;; I need explicit function names to make help-modes useful(use which-key and edit names?), this macro automatically creates them to keep fields as single source of truth
(defmacro sfs--def-prefix-fun (field)
  "Generate a function for inserting FIELD prefix into the query editor."
  `(defun ,(intern (concat "sfs--prefix-" field)) ()
     ,(concat "Add " field ": prefix in query editor.")
     (interactive)
     (insert ,(concat field ":"))))
(dolist (field sfs--fields)
  (eval `(sfs--def-prefix-fun ,field)))

(defvar sfs--editor-widget)
(defun sfs-save-query (id)
  "Save current query to ID."
  (interactive "sEnter ID for query: ")
  (add-to-list 'sfs--collection-favs `(,id . (,(widget-value sfs--editor-widget)))))

(defun sfs--insert-collection (collection)
  "Insert the given COLLECTION into the query editor."
  (interactive "sEnter a collection: ")
  (widget-insert (cdr (assoc collection sfs--collection-all))))

(defvar sfs-query-editor-mode-map)
(setq sfs-query-editor-mode-map
      (let ((map (copy-keymap widget-field-keymap)))
        (define-key map (kbd "<C-return>") 'widget-field-activate)
        (define-key map (kbd "<return>") 'newline-and-indent)
        map))
(general-define-key
 :keymaps 'sfs-query-editor-mode-map
 :prefix "C-c"
 "C-a" '(sfs--prefix-author            :wk "Prefix author:")
 "C-c" '(sfs--prefix-containerfilename :wk "Prefix containerfilename:")
 "C-d" '(sfs--prefix-dir               :wk "Prefix dir:")
 "C-e" '(sfs--prefix-ext               :wk "Prefix ext:")
 "C-f" '(sfs--prefix-filename          :wk "Prefix filename:")
 "C-j" '(sfs--prefix-subject           :wk "Prefix subject:")
 "C-k" '(sfs--prefix-keyword           :wk "Prefix keyword:")
 "C-r" '(sfs--prefix-recipient         :wk "Prefix recipient:")
 "C-l" '(sfs--prefix-rclcat            :wk "Prefix rclcat:")
 "C-m" '(sfs--prefix-mime              :wk "Prefix mime:")
 "C-o" '(sfs--insert-collection        :wk "Insert collection")
 "C-s" '(sfs--prefix-size              :wk "Prefix size:")
 "C-t" '(sfs--prefix-date              :wk "Prefix date:")
 "C-v" '(sfs-save-query                :wk "Save query"))


(define-derived-mode sfs-query-editor-mode prog-mode "SFS"
  "Major mode for SFS interactive query editor.

The query editor uses the Recoll query language, documented here:
https://www.lesbonscomptes.com/recoll/usermanual/webhelp/docs/RCL.SEARCH.LANG.html"
  (setq font-lock-defaults '(sfs-query-highlights))
  )

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
    (setq sfs--logo-section (sfs-section-create top bot))
    )
  )

(defvar sfs--query-editor-section)
(defun sfs-make-section-query-editor ()
  "Make the query section in the sfs TUI."
  (let (top bot editor-widget)
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
                             (sfs-recoll (widget-value sfs--editor-widget))
                             )
                   "Execute")
    (widget-insert "(C-RET)\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (call-interactively #'sfs-save-query)
                             )
                   "Save query"))
  (widget-insert "(C-c C-v)")
  )

(defun sfs--make-query-editor-tui ()
  "Write the tui buffer."
  (let ((buffer (switch-to-buffer "*SFS*")))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (remove-overlays)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (sfs-insert-logo)
      (sfs-make-section-query-editor)
      (keyboard-escape-quit)
      (set-window-point (get-buffer-window "*SFS*")
                        (sfs-section-top sfs--query-editor-section))
      (widget-setup)
      (sfs-query-editor-mode)
      (set-region-read-only (sfs-section-top sfs--logo-section)
                            (sfs-section-bot sfs--logo-section))
      )))

(defun sfs--tui-run (f)
  "Call F in TUI context."
  (call-interactively f)
  (sfs--make-query-editor-tui))

(defun sfs ()
  "Go to SFS start page."
  (interactive)
  (sfs--make-query-editor-tui)
  )

;;; Collections TUI
(define-derived-mode sfs-collections-mode special-mode "SFS Collections"
  "Mode for SFS interactive collections interface."
  (font-lock-mode -1))

(defun sfs--make-section-places ()
  "Make the recent section in the sfs collections TUI."
  (sfs--insert-section-heading "Places")
  (let ((media
         `(("comics" . ,(sfs--queries-or (sfs-split-and-prefix "ext" "djvu cbr cbz cb7 cbt cba")))
           ("books" . ,(sfs--queries-or (sfs-split-and-prefix "ext" "ibooks pdf epub pbd djvu azw azw3 kf8 kfx fb2 mobi opf")))
           ("documents" . ,(sfs--queries-or (sfs-split-and-prefix "ext" "pdf doc docx txt tex")))
           ("music" . "mime:audio")
           ("video" . "mime:video")
           ("text" . "mime:text")))
        )
    (dolist (elmt media)
      (message (cdr elmt))
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (sfs-recoll (cdr elmt)))
                     (car elmt))
      (widget-insert "\n")) ))

;; TODO
;; (defun sfs--make-section-tags ()
;;   "Make the tags section in the sfs collections TUI."
;;   (sfs--insert-section-heading "Tags")
;;   (dolist (elmt sfs--collection-favs)
;;     (widget-create 'push-button
;;                    :notify (lambda (&rest ignore)
;;                              (sfs-recoll (string-join (cadr elmt) " ")))
;;                    (car elmt))
;;     (widget-insert "\n")))

(defun sfs--make-section-recents ()
  "Make the section in the sfs collections TUI for recently touched files."
  (sfs--insert-section-heading "Recent")
  (let ((timespans
         `(("today" . ,(calendar-current-date))
           ("this week" . ,(calendar-gregorian-from-absolute
                            (+ (calendar-absolute-from-gregorian (calendar-current-date)) -7)))
           ("this month" . ,(calendar-gregorian-from-absolute
                             (+ (calendar-absolute-from-gregorian (calendar-current-date)) -30))) ) )
        query-str day month year rtn-date)
    (dolist (elmt timespans)
      (setq rtn-date (cdr elmt))
      (setq day (nth 1 rtn-date))
      (setq month (nth 0 rtn-date))
      (setq year (nth 2 rtn-date))
      (setq query-str (format "date:%d-%d-%d" year month day))
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (sfs-recoll query-str))
                     (car elmt))
      (widget-insert "\n")) ))

(defun sfs--make-section-favorites ()
  "Make the favorites section in the sfs collections TUI."
  (sfs--insert-section-heading "Favorites")
  (dolist (elmt sfs--collection-favs)
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (sfs-recoll (string-join (cadr elmt) " ")))
                   (car elmt))
    (widget-insert "\n")))

(defun sfs--make-collections-tui ()
  "Write the tui buffer."
  (let ((buffer (switch-to-buffer "*SFS Collections*")))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (remove-overlays)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (sfs-insert-logo)
      (widget-insert "\n\n")
      (sfs--make-section-favorites)
      (widget-insert "\n\n")
      (sfs--make-section-recents)
      (widget-insert "\n\n")
      (sfs--make-section-places)
      (sfs-collections-mode)
      (keyboard-escape-quit)
      (widget-setup))))

(defun sfs-collections ()
  "Go to SFS start page."
  (interactive)
  (sfs--make-collections-tui)
  )

(provide 'sfs-tui)
;;; sfs-tui.el ends here
