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
