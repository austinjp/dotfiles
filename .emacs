;; What's in here:

;;  1. Some inspiration.
;;  2. Notes and decisions.
;;  3. Custom variables and faces.
;;  4. Paths where packages are installed.
;;  5. Online package repositories.
;;  6. Initialise package installers.
;;  7. Load custom packages.
;;  8. Language server.
;;  9. Flycheck (LS recommends this over flymake).
;; 10. Flycheck language settings.
;; 11. Mundane bits.

;; ----------------------------------------------------------------------

;; 1. Some inspiration:

;; https://aaronbedra.com/emacs.d/
;; https://github.com/nilcons/emacs-use-package-fast

;; ----------------------------------------------------------------------

;; 2. Notes and decisions.

;; Assumes Emacs version 27.1 on Linux.

;; I run Emacs as a daemon and connect to it with emacsclient, which
;; provides a speedy experience at the expense of a long-running
;; daemon which might in turn start other long-running processes.
;; I use an alias for emacsclient which automatically starts an emacs
;; daemon if there isn't one already running, see here:
;; https://github.com/austinjp/dotfiles/blob/main/.bash_aliases

;; El-Get should be installed by hand, not using package-install.
;; See https://github.com/dimitri/el-get
;; The packaged version works, but uses the deprecated 'cl' package.
;; Also, the packaged version seems to be responsible for impenetrable
;; warning messages about el-get-core not being found.

;; ----------------------------------------------------------------------

;; 3. Custom variables and faces.

;; Automatically generated and updated. Be careful!
;; My approach: if any deleted/uninstalled/non-existent packages are
;; listed in package-selected-packages here, delete them from here
;; and restart Emacs daemon.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-ui lsp-treemacs company lsp-mode xclip markdown-mode flycheck el-get)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ----------------------------------------------------------------------

;; 4. Paths where packages are installed.

(setq custom-paths '("elpa")
      package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; ----------------------------------------------------------------------

;; 5. Online package repositories.

(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") 't)
(add-to-list 'package-archives
	     '("ELPA" . "https://elpa.gnu.org/packages/") 't)

;; ----------------------------------------------------------------------

;; 6. Initialise package installers.

;; This is only needed once, must precede any use of use-package.
;; See https://github.com/nilcons/emacs-use-package-fast
;; and https://github.com/jwiegley/use-package
(setq package-enable-at-startup nil)
(eval-when-compile
  ;; FIXME
  (dolist (p custom-paths)
    (add-to-list 'load-path (expand-file-name p user-emacs-directory)))
  (require 'package)
  ;; (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (use-package use-package
    :config
    (setq use-package-always-ensure 't))
  )

;; ----------------------------------------------------------------------

;; 7. Load custom packages.

;; Using use-package not package, but Emacs inserts package-initialise if
;; it's not in this file commented out!
;; (package-initialize)
(eval-when-compile
  (use-package markdown-mode)
  (use-package xclip)
  )

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

;; FIXME -- this insists on installing package named pkg :(
;;          My Emacs LISP is weak :( :(

;; (require 'use-package)
;; (dolist (pkg custom/packages)
;;   (use-package pkg
;;     :ensure t
;;     ;; :init (global-flycheck-mode)
;;     )
;;   )

;; ----------------------------------------------------------------------

;; 8. Language server.

(use-package lsp-mode
  :commands lsp)

(use-package lsp-ui)

(use-package company
  :init global-company-mode
  )
(add-hook 'after-init-hook #'global-company-mode)

(use-package lsp-treemacs)

;; (use-package helm-lsp)

(require 'lsp-mode)
(setq lsp-mode 't)
(setq lsp-ui-doc-enable nil)
;; Hooks. Note, all are deferred.
(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'lisp-mode-hook #'lsp-deferred)
(add-hook 'sh-mode-hook #'lsp-deferred)

;; (require 'lsp-ui-sideline)
;; (setq lsp-ui-sideline 't)
;; (setq lsp-ui-sideline-mode 't)

;; Use 'complete-anything' (company) for pop-ups.
;; (require 'company)

;; Maybe use this?
;; (use-package dap-mode)

;; Pick one of the following:
;; (use-package helm-lsp)
;; (use-package lsp-ivy)

;; Reminder to myself: alternative way to add hooks:
;; (use-package lsp-mode
;;   :commands lsp
;;   :hook
;;   (sh-mode . lsp))

;; Reminder to myself: to change prefix for lsp-mode keybindings:
;; (setq lsp-keymap-prefix "s-l")


;; ----------------------------------------------------------------------

;; 9. Flycheck.

(setq flycheck-python-flake8-executable "flake8")
(setq flycheck-enabled-checkers '(python-flake8))
;; Prevent Flycheck complaining about various things.
;; Kudos https://emacs.stackexchange.com/a/21666
(setq-default flycheck-disabled-checkers
	      '(
		emacs-lisp-checkdoc ;; this file :)
		lsp                 ;; language server
		)
	      )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ----------------------------------------------------------------------

;; 10. Flycheck language settings.

;; 10 (a). Python.

(add-to-list 'lsp-disabled-clients 'jedi pyls)
(add-to-list 'lsp-enabled-clients 'pylsp)

;; See https://github.com/fredcamps/lsp-jedi
;; (use-package lsp-jedi
;;   :ensure t
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls pylsp)
;;     (add-to-list 'lsp-enabled-clients 'jedi)))

;; ;; Remember, could use mypy here too:
;; ;; (setq flycheck-enabled-checkers '(python-flake8 python-mypy))
;; 
;; ;; To explicitly disable pylint (not currently installed anyway):
;; ;; (setq flycheck-disabled-checkers '(python-pylint)

;; ----------------------------------------------------------------------

;; 11. Mundane bits.

(setq column-number-mode 't)

;; ----------------------------------------------------------------------

;; Notes/experiments below.

;; (dolist (p load-path)
;;   (message (concat "Load path contains: " p)))
