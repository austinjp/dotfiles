;; What's in here:

;;  1. Some inspiration.
;;  2. Notes and decisions.
;;  3. Custom variables and faces.
;;  4. Paths where packages are installed.
;;  5. Online package repositories.
;;  6. Initialise package installers.
;;  7. Load custom packages.
;; . Language server.
;; . Flycheck (LS recommends this over flymake).
;; . Eldoc config.
;; . Flycheck language settings.
;; . Mundane bits.

;; ----------------------------------------------------------------------

;; 1. Some inspiration:

;; https://aaronbedra.com/emacs.d/
;; https://github.com/nilcons/emacs-use-package-fast

;; ----------------------------------------------------------------------

;; 2. Notes and decisions.

;; Assumes Emacs version 27.1 on Linux.

;; To byte-compile ~/.emacs.d directory, kill emacs the run the following.
;; This will speed up subsequent startup times. Note the leading backslash
;; which bypasses any aliases.
;; \emacs -Q --batch --eval '(byte-recompile-directory "~/.emacs.d/" 0)'

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
 '(package-selected-packages '(eglot company xclip markdown-mode flycheck el-get)))

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

;; Company config.

;; Use 'complete-anything' (company) for pop-ups.
(use-package company
  :init (global-company-mode)
  )
(add-hook 'after-init-hook #'global-company-mode)

;; (use-package lsp-treemacs)

;; ----------------------------------------------------------------------

;; Eldoc config.

;; Hmm, try using global-eldoc-mode instead?
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)
(add-hook 'sh-mode-hook 'turn-on-eldoc-mode)

;; ----------------------------------------------------------------------

;; Language server.

(use-package eglot)
(require 'eglot)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       ;; I run pylsp as a systemd service, but that's not necessary.
	       ;; Kudos: https://gitlab.com/morph027/vim8#linux
	       '(emacs-lisp-mode . ("~/.roswell/bin/cl-lsp")))
  (add-to-list 'eglot-server-programs
	       '(lisp-mode . ("~/.roswell/bin/cl-lsp")))
  (add-to-list 'eglot-server-programs
	       '(python-mode . ("pylsp"))
	       ;; "pyflakes3"
	       ;; "jedi-language-server"
	       )
)


;; ;; Reminder to self: This doesn't seem to work, no options appear??
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `(foo-mode . ,(eglot-alternatives
;;                               '(("pylsp")
;;                                 ("~/.local/bin/jedi-language-server/jedi-language-server"))))))

(add-hook 'emacs-lisp-mode-hook 'eglot-ensure)
(add-hook 'ielm-mode-hook 'eglot-ensure)
(add-hook 'lisp-interaction-mode-hook 'eglot-ensure)
(add-hook 'lisp-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'sh-mode-hook 'eglot-ensure)

;; ;; Keep eglot away from some stuff:
;; (add-to-list 'eglot-stay-out-of 'flymake)
;; (add-to-list 'eglot-stay-out-of "company")

;; ----------------------------------------------------------------------

;; Flycheck.

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

;; Flycheck language settings.

;; (a). Python.

;; TODO: mypy

;; ----------------------------------------------------------------------

;; Mundane bits.

(setq column-number-mode 't)

;; ----------------------------------------------------------------------

;; Notes/experiments below.

;; (dolist (p load-path)
;;   (message (concat "Load path contains: " p)))
