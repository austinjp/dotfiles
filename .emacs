;; What's in here:

;; X. Some inspiration.
;; X. Custom variables and faces.
;; X. Path where packages are installed.
;; X. Ensure critical packages are installed.
;; X. List custom packages.
;; X. Misc stuff.
;; X. Package archives.
;; X. Language server.
;; X. Flycheck (LS recommends this over flymake).
;; X. Flycheck for Python.

;; ----------------------------------------------------------------------

;; Some inspiration:
;; https://github.com/nilcons/emacs-use-package-fast

;; ----------------------------------------------------------------------

;; Custom variables and faces.
;; Automatically generated and updated. Be careful!

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(xclip markdown-mode flycheck el-get)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ----------------------------------------------------------------------

;; List of custom packages.
;; Not currently used, see FIXME below.

;; (defvar custom/packages
;;   '(
;;     el-get
;;     flycheck
;;     markdown-mode
;;     use-package
;;     xclip
;;    )
;;   "Default packages"
;;   )

;; ----------------------------------------------------------------------

;; Path where packages are installed.

;; (dolist (p load-path)
;;   (message (concat "Load path contained: " p)))

(setq custom-paths '("elpa")
      package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; ----------------------------------------------------------------------

;; See https://github.com/nilcons/emacs-use-package-fast
(setq package-enable-at-startup nil)
(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

;; ----------------------------------------------------------------------

;; Package repositories.

(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("ELPA" . "https://elpa.gnu.org/packages/") t)

;; ----------------------------------------------------------------------

;; Ensure critical packages are installed

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; 
;; (unless (package-installed-p 'el-get)
;;   (package-refresh-contents)
;;   (package-install 'el-get))

;; ----------------------------------------------------------------------

(eval-when-compile
  ;; This is only needed once, must precede any use of use-package.
  ;; See https://github.com/jwiegley/use-package
  (dolist (p custom-paths)
    (add-to-list 'load-path (expand-file-name p user-emacs-directory)))

  ;; (require 'el-get)
  ;; (el-get 'sync)

  (require 'use-package)
  )


;; (dolist (p load-path)
;;   (message (concat "Load path now contains: " p)))

;; ----------------------------------------------------------------------

;; Load custom packages.

;; Using use-package not package, but Emacs inserts package-initialise if
;; it's not in this file commented out!
;; (package-initialize)

(use-package markdown-mode :ensure t)
(use-package xclip :ensure t)

;; FIXME -- this insists on installing package named pkg :(
;; (require 'use-package)
;; (dolist (pkg custom/packages)
;;   (use-package pkg
;;     :ensure t
;;     ;; :init (global-flycheck-mode)
;;     )
;;   )

;; ----------------------------------------------------------------------

;; Language server.

;; ----------------------------------------------------------------------

;; Flycheck.

;; Prevent Flycheck complaining about various things.
;; Kudos https://emacs.stackexchange.com/a/21666
(setq-default flycheck-disabled-checkers
	      '(
		emacs-lisp-checkdoc ;; for this file :)
		lsp                 ;; language server
		)
	      )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ----------------------------------------------------------------------

;; Flycheck for Python.

;; (setq flycheck-python-flake8-executable "flake8")
;; (setq flycheck-enabled-checkers '(python-flake8))
;; 
;; ;; Remember, could use mypy here too:
;; ;; (setq flycheck-enabled-checkers '(python-flake8 python-mypy))
;; 
;; ;; To explicitly disable pylint (not currently installed anyway):
;; ;; (setq flycheck-disabled-checkers '(python-pylint)

;; ----------------------------------------------------------------------

;; Mundane bits.

(setq column-number-mode 't)

;; ----------------------------------------------------------------------

;; Notes/experiments below.

;; Nothing to see here!
