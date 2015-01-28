(prefer-coding-system 'utf-8)
(add-to-list 'exec-path "/usr/local/bin")
(load (expand-file-name "~/.emacs.d/keymaps.el"))

(when (display-graphic-p)
    (set-frame-size (selected-frame) 172 60))

(tool-bar-mode -1)




;; add Melpa to the package source
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
)

;; init packages
(setq packages-list '(ack cider-browse-ns cider queue pkg-info epl dash clojure-mode cider-decompile javap-mode cider queue pkg-info epl dash clojure-mode cider-spy dash cider queue pkg-info epl dash clojure-mode cider-tracing clojure-mode cider queue pkg-info epl dash clojure-mode company-cider cider queue pkg-info epl dash clojure-mode company fish-mode helm-ack helm async helm-company company helm async helm-flycheck helm async flycheck pkg-info epl dash dash helm-projectile projectile pkg-info epl dash f dash s s helm async javap-mode markdown-mode org projectile pkg-info epl dash f dash s s pungi jedi python-environment deferred auto-complete popup epc ctable concurrent deferred python-environment deferred queue rainbow-delimiters rust-mode s smartparens dash yaml-mode yasnippet zenburn-theme))

(unless package-archive-contents
    (package-refresh-contents))
(dolist (package packages-list)
    (unless (package-installed-p package)
          (package-install package))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: helm              ;;
;;                            ;;
;; GROUP: Convenience -> Helm ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm)

;; must set before helm-config,  otherwise helm use default
;; prefix "C-x c", which is inconvenient because you can
;; accidentially pressed "C-x C-c"
(setq helm-command-prefix-key "C-c h")

(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)
(setq
 helm-google-suggest-use-curl-p t
 helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
 helm-quick-update t ; do not display invisible candidates
 helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
 helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

 helm-split-window-default-side 'other ;; open helm buffer in another window
 helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
 helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                     '(picture-mode artist-mode))
 helm-candidate-number-limit 200 ; limit the number of displayed canidates
 helm-M-x-requires-pattern 0     ; show all candidates when set to 0
 helm-boring-file-regexp-list
 '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
 helm-ff-file-name-history-use-recentf t
 helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
 ido-use-virtual-buffers t      ; Needed in helm-buffers-list
 helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
                                        ; useful in helm-mini that lists buffers
 )
(global-set-key (kbd "M-x") 'helm-M-x)

;; Save current position to mark ring when jumping to a different place
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(helm-mode 1)

;; Evil mode
(add-to-list 'load-path "~/.emacs.d/evil")  
(require 'evil)  
(evil-mode 1)
(add-hook 'term-mode-hook 'evil-emacs-state)
(add-hook 'repl-mode-hook 'evil-emacs-state)
(add-hook 'cider-repl-mode-hook 'evil-emacs-state)

;; YASNippet

(require 'yasnippet)
(yas-global-mode 1)

;; improved autocompletion mechanism
(add-hook 'after-init-hook 'global-company-mode)

;; better linter
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Projectile : project handling
(projectile-global-mode)

;; SmartParens : alternative to paredit
(load (expand-file-name "~/.emacs.d/SP.el"))

;; rainbow-delimiters 

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; related to Cider (clojure mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
