;;; .emacs --- My .emacs file.

;;; Commentary:
;; 
;; What's in here:
;;
;;  1. Some inspiration.
;;  2. Notes and decisions.
;;  3. Custom variables and faces.
;;  4. Paths and package installation.
;;  5. Online package repositories.
;;  6. Load custom packages.
;;  7. Eglot config.
;;  8. Eldoc config.
;;  9. Golang config.
;; 10. Miscellaneous.
;;

;;; Code:

;; ======================================================================

;; 1. Some inspiration:

;; https://aaronbedra.com/emacs.d/
;; https://github.com/nilcons/emacs-use-package-fast
;; https://github.com/martenlienen/dotfiles/blob/e4f7c47/home/.emacs.d/straight/repos/mlextras/ml-init.org

;; ======================================================================

;; 2. Notes and decisions.

;; Assumes Emacs version 27.1 on Linux.

;; To byte-compile ~/.emacs.d directory, kill Emacs the run the following.
;; This will speed up subsequent startup times. Note the leading backslash
;; which bypasses any aliases.
;; \emacs -Q --batch --eval '(byte-recompile-directory "~/.emacs.d/" 0)'

;; I run Emacs as a daemon and connect to it with emacsclient, which
;; provides a speedy experience at the expense of a long-running
;; daemon which might in turn start other long-running processes.
;; I use an alias for emacsclient which automatically starts an Emacs
;; daemon if there isn't one already running, see here:
;; https://github.com/austinjp/dotfiles/blob/main/.bash_aliases

;; There is much debate about flymake vs flycheck:
;; https://github.com/purcell/flymake-flycheck

;; ======================================================================

;; 3. Custom variables and faces.

;; Automatically generated and updated. Be careful!
;; My approach: if any deleted/uninstalled/non-existent packages are
;; listed in package-selected-packages here, delete them from here
;; and restart Emacs daemon.

;; (setq x-select-enable-clipboard t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(prettier-rc prettier-js prettier biomejs-format lsp-mode go-mode jinx dockerfile-mode noxml-fold cargo-mode cargo rust-mode cython-mode eldoc multiple-cursors basic-mode fold-this haxe-mode lua-mode magit js2-mode flymake-css yaml-mode undo-tree rainbow-delimiters eglot sideline-flymake sideline markdown-mode multi-web-mode json-mode company cmake-mode))
 '(undo-limit 10000)
 '(undo-tree-limit 10000)
 '(undo-tree-outer-limit 10000)
 '(undo-tree-strong-limit 10000))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((((type tty)) (:foreground "cyan"))))
 '(font-lock-function-name-face ((t (:foreground "color-33"))))
 '(font-lock-string-face ((((type tty)) (:foreground "blue"))))
 '(jinx-highlight ((t (:inherit isearch :background "red"))))
 '(jinx-misspelled ((t (:inherit error :foreground "color-178" :underline t))))
 '(markdown-comment-face ((t (:inherit font-lock-comment-face :foreground "color-201"))))
 '(markdown-markup-face ((t (:inherit shadow :foreground "color-245" :slant normal :weight normal))))
 '(minibuffer-prompt ((((type tty)) (:foreground "cyan"))))
 '(mode-line-buffer-id ((t (:foreground "color-232" :weight bold))))
 '(rainbow-delimiters-base-error-face ((t (:inherit rainbow-delimiters-base-face :foreground "red"))))
 '(rainbow-delimiters-base-face ((t (:inherit nil))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "color-40"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "color-208"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-depth-1-face))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-depth-2-face))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-depth-3-face))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-depth-1-face))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-depth-2-face))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-depth-3-face))))
 '(rainbow-delimiters-mismatched-face ((t (:inherit rainbow-delimiters-unmatched-face)))))

;; ======================================================================

;; 4. Paths and package installation.

;; Ensure we're using use-package since it automatically installs
;; missing packages. The following commented-out line must be left intact
;; or Emacs will insert it and initialise the "package" package.

;; (package-initialize)

(setq package-enable-at-startup nil)
(require 'package)

(eval-when-compile
  (setq custom-paths '())
  (dolist (p '("elpa" "packages" "site-lisp"))
    (add-to-list 'custom-paths (expand-file-name p user-emacs-directory))
    (add-to-list 'load-path (expand-file-name p user-emacs-directory))
    )
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    )
  )
(require 'use-package)
(use-package use-package
  :config
  (setq use-package-always-ensure 't))

;; Kick-start a bunch of packages:

(require 'lorem-ipsum)
;; (require 'cython-mode)

;; Associate some filename endings with modes.
; Cython mode
(add-to-list 'auto-mode-alist '("\\.pyx"   . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd"   . cython-mode))

;; ; Quarto mode.
;; (add-to-list 'auto-mode-alist '("\\.qmd"   . quarto-mode))
;; (require 'quarto-mode)

; JavaScript mode.
(add-to-list 'auto-mode-alist '("\\.json5" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.mjs"   . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.avsc"  . javascript-mode))
; Go-lang mode.
(add-to-list 'auto-mode-alist '("\\.go"    . go-mode))
; Using sh-mode for direnv files.
(add-to-list 'auto-mode-alist '("\\.envrc"    . sh-mode))

;; ======================================================================

;; 5. Online package repositories.

(add-to-list 'package-archives
             '("MELPA" . "https://melpa.org/packages/") 't)
;; (add-to-list 'package-archives
;;              '("MELPA Stable" . "https://stable.melpa.org/packages/") 't)
(add-to-list 'package-archives
             '("ELPA" . "https://elpa.gnu.org/packages/") 't)

;; ======================================================================

;; 6. Load custom packages.

;; (a) Complete-anything (company).
;; Kudos https://github.com/martenlienen/dotfiles/blob/e4f7c47/home/.emacs.d/straight/repos/mlextras/ml-init.org

(use-package company
  :init
  (global-company-mode)
  (setf
   company-idle-delay 0
   company-minimum-prefix-length 2
   company-show-quick-access t
   company-selection-wrap-around t
   )
  )

;; (b) Sideline to display messages from Flymake.

(use-package sideline-flymake)
(use-package sideline
  :hook
  (flymake-mode-hook . sideline-mode)
  :init
  (global-sideline-mode)
  (setq
   sideline-backends-right '(sideline-flymake)
   sideline-flymake-display-errors-whole-line 'line ; or 'point to show errors only on point
   ;; sideline-backends-skip-current-line t         ; don't display on current line
   sideline-order-right 'down                       ; or 'up
   sideline-format-left "%s   "                     ; format for left aligment
   sideline-format-right "   %s"                    ; format for right aligment
   sideline-priority 100                            ; overlays' priority
   sideline-display-backend-name 'f                 ; display the backend name
   )
  )

;; (c) Flymake bits and bobs.

(use-package flymake-css)

;; (d) Misc.

;; (use-package web-mode :mode "\\.html\\.?")
(use-package js2-mode)
(use-package json-mode)
(use-package multi-web-mode)
(use-package markdown-mode)
(use-package yaml-mode)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (setf rainbow-delimiters-max-face-count 6)
  )

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  )

(setq undo-tree-auto-save-history t)

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; ======================================================================

;; 7. Eglot config. Currently disabled!

;; (use-package eglot)
;; 
;; ;; Tell Eglot which language servers are available.
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(js-mode . ("typescript-language-server" "--stdio")))
;;   (add-to-list 'eglot-server-programs
;;                '(js2-mode . ("typescript-language-server" "--stdio")))
;;   (add-to-list 'eglot-server-programs
;;                '(json-mode . ("typescript-language-server" "--stdio")))
;;   ;; (add-to-list 'eglot-server-programs
;;   ;;              '(python-mode . ("pylsp")))
;;   (add-to-list 'eglot-server-programs
;;                '(sh-mode . ("bash-language-server" "start")))
;;   (add-to-list 'eglot-server-programs
;;                '(shell-script-mode . ("bash-language-server" "start")))
;;   )
;; 
;; ;; Enable generally:
;; ;; (add-hook 'prog-mode-hook 'eglot-ensure)
;; 
;; ;; Alternatively enable individually:
;; (add-hook 'sh-mode-hook 'eglot-ensure)
;; (add-hook 'shell-script-mode-hook 'eglot-ensure)
;; ;; (add-hook 'python-mode-hook 'eglot-ensure)
;; ;; (add-hook 'js-mode-hook 'eglot-ensure)
;; ;; etc.

;; Keep eglot away from some stuff:
;; (add-to-list 'eglot-stay-out-of 'flymake)
;; (add-to-list 'eglot-stay-out-of "company")

;; ======================================================================

;; 8. Eldoc config.

;; Prevent eldoc expanding the echo area.
;; (setq eldoc-echo-area-use-multiline-p nil)

(use-package eldoc
  :config
  (setf eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p nil)
  )

;; ======================================================================

;; 9. Golang config.

;; Automatically run gofmt on save.
(add-hook 'before-save-hook 'gofmt-before-save)

;; Automatically run goimports on save.
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; ======================================================================

;; 10. Miscellaneous.

;; Kudos https://github.com/martenlienen/dotfiles/blob/e4f7c47/home/.emacs.d/straight/repos/mlextras/ml-init.org

;; Line breaks at 88 characters, default for black.
(setq-default fill-column 88)

;; Single space after a full-stop.
(setq-default sentence-end-double-space nil)

;; Append a newline at the end of files
(setq require-final-newline t)

;; Remove trailing whitespace. Kudos https://github.com/fxbois/web-mode/issues/283#issuecomment-47773112
(add-hook 'local-write-file-hooks (lambda () (delete-trailing-whitespace) nil))

;; Enable X clipboards
(setf x-select-enable-clipboard t
      x-select-enable-primary t)

;; Configure the *scratch* buffer
(setf initial-scratch-message "")

;; Show column numbers.
(column-number-mode t)

;; y and n instead of yes and no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Never re-center during scrolling.
(setq scroll-conservatively 101)

;; Keep some distance between point and the window margin
(setq scroll-margin 3)

;; Key-bindings for window resizing.
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Backups.
(setq backup-by-copying t      ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; (setq backup-directory-alist
;;       `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))

(setq backup-directory-alist
       '(("." . "~/.local/tmp/")))
(setq auto-save-file-name-transforms
       `((".*" ,"~/.local/tmp/" t)))

;; Indentation.
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if        
  ;; neither, we use the current indent-tabs-mode                               
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

;; Copy to X clipboard using M-c (left alt c).
(defun copy-selected (beg end)
  (interactive "*r")
  (if (region-active-p)
      (shell-command-on-region beg end "xsel -b")
    (message "No selection!")))
(global-set-key (kbd "M-c") 'copy-selected)

;; Hide/show menu bar using F10.
(menu-bar-mode -1) ; start hidden
(global-set-key [f10] 'toggle-menu-bar-mode-from-frame)

;; Multiple cursors.
(global-set-key (kbd "C-M-j") 'mc/mark-all-dwim)

;; Allow emacs and emacsclient to respond to mouse.
;; Kudos: https://stackoverflow.com/a/6798279
(defun my-terminal-config (&optional frame)
  "Establish settings for the current terminal."
  (if (not frame) ;; The initial call.
      (xterm-mouse-mode 1)
    ;; Otherwise called via after-make-frame-functions.
    (if xterm-mouse-mode
        ;; Re-initialise the mode in case of a new terminal.
        (xterm-mouse-mode 1))))
;; Evaluate both now (for non-daemon emacs) and upon frame creation
;; (for new terminals via emacsclient).
(my-terminal-config)
(add-hook 'after-make-frame-functions 'my-terminal-config)

;; Prettier for JavaScript.
(require 'prettier-js)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'multi-web-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

;; ;; Biome for JavaScript formatting.
;; (require 'biomejs-format)
;; (add-hook 'js-mode-hook 'biomejs-format-mode)
;; (add-hook 'js2-mode-hook 'biomejs-format-mode)
;; ;; (add-hook 'web-mode-hook 'biomejs-format-mode)
;; ;; (add-hook 'multi-web-mode-hook 'biomejs-format-mode)
;; (setq biomejs-format-biome-args
;;       '(
;;         "format"
;;         "--trailing-comma" "all"
;;         "--bracket-spacing" "true"
;;         "--javascript-formatter-indent-width" "4"
;;         "--indent-style" "space"
;;         )
;;       )

;; ======================================================================
