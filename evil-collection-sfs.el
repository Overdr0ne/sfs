;;; evil-collection-sfs.el --- Evil bindings and configuration for SFS.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defun evil-collection-sfs-setup ()
  "Sets up keybindings and initial states for SFS."
  (interactive)
  (evil-set-initial-state 'sfs-research-mode 'insert)

  (evil-collection-define-key '(normal insert) 'sfs-research-mode-map
    (kbd "C-q")        'quit-window
    (kbd "<C-return>") 'widget-field-activate
    (kbd "C-r")        'sfs--record-query)
  (evil-collection-define-key 'normal 'sfs-recollect-mode-map
    (kbd "<return>") 'sfs-recollect-at-point
    (kbd "q")        'quit-window)
  (evil-collection-define-key 'normal 'sfs-represent-mode-map
    (kbd "q")        'quit-window)
  (evil-collection-define-key 'normal 'sfs-redir-mode-map
    (kbd "<C-return>") 'sfs-represent
    (kbd "q") 	       'quit-window))

(provide 'evil-collection-sfs)
;;; evil-collection-sfs.el ends here
