;; What's in here:

;;  1. Some inspiration.
;;  2. Notes and decisions.
;;  3. Custom variables and faces.
;;  4. Paths where packages are installed.
;;  5. Online package repositories.
;;  6. Initialise package installers.
;;  7. Load custom packages.
;;  8. Eldoc config.
;;  9. Language server.
;; 10. Flycheck (LS recommends this over flymake).
;; 11. Flycheck language settings.
;; 12. Miscellaneous.

;; ======================================================================

;; 1. Some inspiration:

;; https://aaronbedra.com/emacs.d/
;; https://github.com/nilcons/emacs-use-package-fast

;; ======================================================================

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

;; There is much debate about flymake vs flycheck. I've gone with flycheck
;; but... see https://github.com/purcell/flymake-flycheck

;; ======================================================================

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
 '(help-at-pt-timer-delay 0.1)
 '(package-selected-packages
   '(sideline-flymake multi-web-mode json-mode treemacs-magit treemacs magit sideline-flycheck sideline project eglot company xclip markdown-mode flycheck)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minibuffer-prompt ((((type tty)) (:foreground "green"))))
 '(font-lock-string-face ((((type tty)) (:foreground "blue"))))
 '(font-lock-builtin-face ((((type tty)) (:foreground "cyan"))))
 )

;; ======================================================================

;; 4. Paths where packages are installed.

(setq custom-paths '("elpa")
      package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; ======================================================================

;; 5. Online package repositories.

(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") 't)
(add-to-list 'package-archives
             '("ELPA" . "https://elpa.gnu.org/packages/") 't)

;; ======================================================================

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

;; ======================================================================

;; 7. Load custom packages.

;; Using use-package not package, but Emacs inserts package-initialise if
;; it's not in this file commented out!
;; (package-initialize)
(eval-when-compile
  (use-package company
    :init
    (global-company-mode)
    )
  ;; (use-package company-quickhelp) ;; see config below
  (use-package eglot)             ;; see config below
  (use-package flycheck           ;; see config below
    :init
    (global-flycheck-mode)
    )
  (use-package json-mode)
  (use-package magit)
  (use-package multi-web-mode)
  (use-package markdown-mode)
  (use-package project)
  (use-package treemacs-magit)
  (use-package treemacs)
  (use-package xclip)
  )

;; Use sideline and sideline-flycheck to display messages from flycheck.
(use-package sideline
  :init
  (global-sideline-mode)
  (setq
   sideline-backends-right '(sideline-flycheck)
   ;; sideline-backends-skip-current-line t  ; don't display on current line
   sideline-order-right 'down                ; or 'up
   sideline-format-left "%s   "              ; format for left aligment
   sideline-format-right "   %s"             ; format for right aligment
   sideline-priority 100                     ; overlays' priority
   sideline-display-backend-name t           ; display the backend name
   )
  )
(use-package sideline-flycheck
  :init
  (sideline-flycheck-mode)
  :hook
  (flycheck-mode-hook . sideline-mode)
  (flycheck-mode-hook . sideline-flycheck-setup)
  )

;; Ensure the following are always active.
(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook #'global-sideline-mode)
(add-hook 'after-init-hook #'sideline-flycheck-mode)
(sideline-flycheck-mode)

;; My key-bindings for company-quickhelp override one for flycheck,
;; so they're set up after flycheck, below.

;; The below is Not currently used, see FIXME.

;; (defvar custom/packages
;;   '(
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

;; ======================================================================

;; 8. Eldoc config.

;; Prevent eldoc expanding the echo area.
(setq eldoc-echo-area-use-multiline-p nil)

;; Hmm, try global-eldoc-mode instead?
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'python-mode-hook 'turn-on-eldoc-mode)
(add-hook 'sh-mode-hook 'turn-on-eldoc-mode)

;; ======================================================================

;; 9. Language server.

(require 'eglot)

(with-eval-after-load 'eglot
  ;; Unfortunately cl-lsp seems to crash on startup :(
  ;; (add-to-list 'eglot-server-programs
  ;;              '(emacs-lisp-mode . ("~/.roswell/bin/cl-lsp")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '(lisp-mode . ("~/.roswell/bin/cl-lsp")))
  (add-to-list 'eglot-server-programs
               ;; I run pylsp as a systemd service, but that's not necessary.
               ;; Kudos: https://gitlab.com/morph027/vim8#linux
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

;; See above, cl-lsp seems to crash on startup.
;; (add-hook 'emacs-lisp-mode-hook 'eglot-ensure)
;; (add-hook 'ielm-mode-hook 'eglot-ensure)
;; (add-hook 'lisp-interaction-mode-hook 'eglot-ensure)
;; (add-hook 'lisp-mode-hook 'eglot-ensure)

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'sh-mode-hook 'eglot-ensure)

;; ;; Keep eglot away from some stuff:
;; (add-to-list 'eglot-stay-out-of 'flymake)
;; (add-to-list 'eglot-stay-out-of "company")

;; ======================================================================

;; 10.Flycheck.

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

(add-hook 'after-init-hook #'global-flycheck-mode)

;; ======================================================================

;; 11. Flycheck language settings.

;; (a). Python.

;; TODO: mypy

;; ======================================================================

;; 12. Miscellaneous.

(setq column-number-mode 't)
(menu-bar-mode -1)
(ido-mode -1)

;; ······································································

;; Backups.

(setq backup-by-copying t      ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 6
      version-control t)       ; use versioned backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; ······································································

;; Indents.

(setq-default indent-tabs-mode nil)

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if        
  ;; neither, we use the current indent-tabs-mode                               
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun my-setup-indent (n)
  ;; java/c/c++
  (setq-default c-basic-offset n)
  ;; web development
  ;; (setq-default coffee-tab-width n) ; coffeescript
  (setq-default javascript-indent-level n) ; javascript-mode
  (setq-default js-indent-level n) ; js-mode
  (setq-default js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-default web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-default web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-default web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-default css-indent-offset n) ; css-mode
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  )
(my-setup-indent 2)

;; ······································································

;; Enable font lock for LaTeX maths expressions.

(setq-default markdown-enable-math t)

;; ······································································

;; Key-bindings for window resizing.

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; ······································································

;; Tweak column widths when listing packages!!!
;; Kudos https://stackoverflow.com/a/28386980

(defcustom package-menu-column-width 20
  "Width of the package column."
  :type 'number
  :group 'package)

(define-derived-mode package-menu-mode tabulated-list-mode "Package Menu"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
  (setq tabulated-list-format
        `[("Package" ,package-menu-column-width package-menu--name-predicate)
          ("Version" 12 nil)
          ("Status"  10 package-menu--status-predicate)
          ,@(if (cdr package-archives)
                '(("Archive" 16 package-menu--archive-predicate)))
          ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (add-hook 'tabulated-list-revert-hook 'package-menu--refresh nil t)
  (tabulated-list-init-header))

;; ······································································

;; company-quickhelp

;; '(define-key flycheck-mode "C-c h" nil)
;; '(define-key flycheck "C-c h" nil)
;; (eval-after-load 'company
;;   '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))
;; (eval-after-load 'flycheck
;;   '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))
;; (eval-after-load 'flycheck-mode
;;   '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

;; ======================================================================

;; Notes/experiments below.

;; (dolist (p load-path)
;;   (message (concat "Load path contains: " p)))
