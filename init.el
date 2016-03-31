;;; init.el --- custom init

;;; Commentary:
;;; use-package for configuration and ensuring everything is loaded/dowloaded
;;; Use Evil for modal editing,
;;; Helm to provide a better minibuffer
;;; Cider for Clojure, complete with hooks and boot support
;;; Smartparens for pair completion/handling
;;; Company for autocompletion
;;; Flycheck for basic code linting
;;; Projectile for project management
;;; slime/sbcl configuration
;;; Zenburn as theme.

;;; Code:

(prefer-coding-system 'utf-8)
(add-to-list 'exec-path "/usr/local/bin")
(load (expand-file-name "~/.emacs.d/keymaps.el"))
(load (expand-file-name "~/.emacs.d/org.el"))

(when (display-graphic-p)
    (set-frame-size (selected-frame) 172 60))

(tool-bar-mode -1)

;; add Melpa to the package source
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;; init packages that can't be maintained with use-package
(setq packages-list '(org use-package zenburn-theme))

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package packages-list)
  (unless (package-installed-p package)
    (package-install package))) 

(use-package paradox
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: helm              ;;
;;                            ;;
;; GROUP: Convenience -> Helm ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :ensure t
  :init
  ;; must set before helm-config,  otherwise helm use default
  ;; prefix "C-x c", which is inconvenient because you can
  ;; accidentially pressed "C-x C-c"
  (setq helm-command-prefix-key "C-c h")
  :config
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

  (helm-mode 1))

;; magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package relative-line-numbers
  :ensure t
  :config
  (global-relative-line-numbers-mode))

;; Evil mode
(use-package evil
  :ensure t
  :init
  ;; Deactivate evil mode for some buffer. Can be quite annoying.
  (add-hook 'term-mode-hook 'evil-emacs-state)
  (add-hook 'repl-mode-hook 'evil-emacs-state)
  (add-hook 'paradox-menu-mode-hook 'evil-emacs-state)
  (add-hook 'cider-repl-mode-hook 'evil-emacs-state)
  :config
  (use-package evil-easymotion
    :ensure t)
  (use-package evil-magit
    :ensure t)
  (evil-mode 1))

;; YASNippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (use-package clojure-snippets
    :ensure t))

;; improved autocompletion mechanism
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (use-package helm-company
    :ensure t))

;; better linter
(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (use-package helm-flycheck
    :ensure t))

;; Projectile : project handling
(use-package projectile
  :ensure t
  :config
  (use-package helm-projectile
    :ensure t)
  (projectile-global-mode))

;; SmartParens : alternative to paredit
(use-package smartparens
  :ensure t
  :config
  (use-package evil-smartparens
    :ensure t)
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (load (expand-file-name "~/.emacs.d/SP.el")))

(use-package rainbow-delimiters
  :ensure t
  :init 
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; related to Cider (clojure mode)
(use-package cider
  :ensure t
  :init
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  :config
  (show-smartparens-global-mode +1))

(use-package clj-refactor
  :ensure t
  :config
  (require 'clj-refactor)
  (add-hook 'clojure-mode-hook (lambda ()
				 (clj-refactor-mode 1)
				 ;; insert keybinding setup here
				 )))

(use-package rainbow-delimiters
  :ensure t) 

;; file type mode
(use-package jedi
  :ensure t)
(use-package haskell-mode
  :ensure t)
(use-package erlang
  :ensure t)
(use-package rust-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package fish-mode
  :ensure t)

;; various usefull package 
(use-package ace-jump-mode
  :ensure t) 

(use-package ack
  :ensure t
  :config
  (use-package helm-ack
    :ensure t))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" "20e359ef1818a838aff271a72f0f689f5551a27704bf1c9469a5c2657b417e6c" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "85c59044bd46f4a0deedc8315ffe23aa46d2a967a81750360fb8600b53519b8a" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "a444b2e10bedc64e4c7f312a737271f9a2f2542c67caa13b04d525196562bf38" "2e5705ad7ee6cfd6ab5ce81e711c526ac22abed90b852ffaf0b316aa7864b11f" "95a6ac1b01dcaed4175946b581461e16e1b909d354ada79770c0821e491067c6" "b06aaf5cefc4043ba018ca497a9414141341cb5a2152db84a9a80020d35644d1" "3dafeadb813a33031848dfebfa0928e37e7a3c18efefa10f3e9f48d1993598d3" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "989829fc49ea2643a4e018bf8e082a92b25f2d76b18754db5f47d0e40c611386" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "3c093ea152d7185cc78b61b05e52648c6d2fb0d8579c2119d775630fa459e0be" "cbef37d6304f12fb789f5d80c2b75ea01465e41073c30341dc84c6c0d1eb611d" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" default)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; put backup file in the temp directory... avoid so many complication.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(provide 'init)
;;; init.el ends here

