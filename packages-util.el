;;; package --- handle package installed

(setq packages-list '())

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every package not installed."
  (mapcar
   (lambda (package)
     (package-installed-p 'evil)
     (if (package-installed-p package)
         package
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         nil)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'iedit 'magit) ;  --> (nil nil) if iedit and magit are already installed

;; activate installed packages
(package-initialize)

(defun update-package-list ()
 "Update the package list in this file with the one we've downloaded"
 (goto-char (point-min))
 (search-forward "packages-list '(")
 (kill-sexp)
 (insert "test"))


