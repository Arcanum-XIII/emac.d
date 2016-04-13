;;; org.el -- config for org-mode
;;; Commentary:
;;; Code
(setq org-todo-keywords
       '((sequence "TODO" "DOING" "DONE")))

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "/codex.org"))
(setq org-hide-leading-stars t)
(define-key global-map "\C-cc" 'org-capture)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))
(provide 'org)
;;; org.el ends here
