;;; keymaps --- provide special configuration for key 

;;; Commentary:

;;; Code:

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'nil)

(global-set-key (kbd "M-v") 'clipboard-yank)
(global-set-key (kbd "M-c") 'clipboard-kill-ring-save)

(provide 'keymaps)\n;;; keymaps.el ends here
