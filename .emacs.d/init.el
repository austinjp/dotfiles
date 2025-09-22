;;; .emacs --- My .emacs file.

;;; Commentary:
;;
;; What's in here:
;;
;;  1. Some inspiration.
;;  2. Notes and decisions.
;;  3. Custom variables and faces.
;;  4. Paths and package installation.
;;  5. Language modes.
;;  6. Online package repositories.
;;  7. Load custom packages.
;;  8. Eglot config.
;;  9. Eldoc config.
;; 10. Golang config.
;; 11. Miscellaneous.
;;

;;; Code:

;; ======================================================================

;; 1. Some inspiration:

;; https://aaronbedra.com/emacs.d/
;; https://github.com/nilcons/emacs-use-package-fast
;; https://github.com/martenlienen/dotfiles/blob/e4f7c47/home/.emacs.d/straight/repos/mlextras/ml-init.org

;; ======================================================================

;; 2. Notes and decisions.

;; Assumes Emacs version 29.3 on Linux.

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
 '(go-ts-mode-indent-offset 4)
 '(indent-tabs-mode t)
 '(package-selected-packages
   '(jq-mode jq-ts-mode web-mode multi-web-mode nginx-mode d2-mode terraform-mode yaml-mode eglot nerd-icons-completion nerd-icons-corfu corfu-terminal kind-icon corfu quelpa treesit-fold treesit protobuf-mode basic-mode exec-path-from-shell fold-this gnu-elpa-keyring-update markdown-mode noxml-fold rainbow-delimiters undo-tree))
 '(tab-width 4)
 '(undo-limit 10000)
 '(undo-tree-limit 10000)
 '(undo-tree-outer-limit 10000)
 '(undo-tree-strong-limit 10000))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((((type tty)) (:foreground "royalblue3"))))
 '(font-lock-function-name-face ((t (:foreground "green"))))
 '(font-lock-keyword-face ((((type tty)) (:foreground "green4"))))
 '(font-lock-string-face ((((type tty)) (:foreground "purple"))))
 '(markdown-comment-face ((t (:inherit font-lock-comment-face :foreground "coral1"))))
 '(markdown-markup-face ((t (:inherit shadow :foreground "royalblue1" :slant normal :weight normal))))
 '(minibuffer-prompt ((((type tty)) (:foreground nil))))
 '(mode-line-buffer-id ((t (:foreground nil :weight bold))))
 '(rainbow-delimiters-base-error-face ((t (:inherit rainbow-delimiters-base-face :foreground "brightred"))))
 '(rainbow-delimiters-base-face ((t (:inherit nil))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "green"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan4"))))
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

;; The following are an attempt at getting node (via nvm) into the PATH
;; so prettier can run it. See https://github.com/purcell/exec-path-from-shell
(require 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))

(dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG"
	       "LANGUAGE" "LC_CTYPE" "PATH" "NODE_PATH"))
  (add-to-list 'exec-path-from-shell-variables var))

;; End of prettier/node/nvm path stuff.

(eval-when-compile
  (setq custom-paths '())
  (dolist (p '("elpa" "packages" "site-lisp"))
    (add-to-list 'custom-paths (expand-file-name p
						 user-emacs-directory))
    (add-to-list 'load-path (expand-file-name p user-emacs-directory)))
  (unless
      (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Kick-start a few packages:

(require 'use-package)

(use-package use-package
  :config
  (setq use-package-always-ensure 't))

(require 'lorem-ipsum)

;; ======================================================================

;; 5. Language modes.

;; (require 'cython-mode)
;; Associate some filename endings with modes.
;; Cython mode
;; (add-to-list 'auto-mode-alist '("\\.pyx"   . cython-mode))
;; (add-to-list 'auto-mode-alist '("\\.pxd"   . cython-mode))

;; Quarto mode.
;; (add-to-list 'auto-mode-alist '("\\.qmd"   . poly-quarto-mode))
;; (require 'quarto-mode)

;; JavaScript, JSON, Typescript etc modes.
(add-to-list 'auto-mode-alist '("\\.mjs"   . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json5" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.avsc"  . js-ts-mode))

(add-to-list 'auto-mode-alist '("\\.ts"    . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx"   . tsx-ts-mode))


;; Go-lang modes.
(add-to-list 'auto-mode-alist '("\\.go"    . go-ts-mode))
(add-to-list 'auto-mode-alist '("go.mod"   . go-mod-ts-mode))

;; Using sh-mode for direnv files.
(add-to-list 'auto-mode-alist '("\\.envrc" . sh-mode))

;; HTML mode.
(add-to-list 'auto-mode-alist '("\\.html"  . html-mode))

;; Javascript mode.
(add-to-list 'auto-mode-alist '("\\.html"  . html-mode))

;; Catch-all: remap modes that aren't explicitly triggered by auto-mode-alist.
(setq major-mode-remap-alist
      '(
        (yaml-mode . yaml-ts-mode)
        (javascript-mode . js-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)
        )
      )

;; ======================================================================

;; 5. Online package repositories.

(add-to-list 'package-archives
	     '("MELPA" . "https://melpa.org/packages/") 't)

;; (add-to-list 'package-archives
;;              '("MELPA Stable" . "https://stable.melpa.org/packages/") 't)
(add-to-list 'package-archives
	     '("ELPA" . "https://elpa.gnu.org/packages/") 't)

;; Language repos for treesitter.
;; Courtesy https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; and https://github.com/mickeynp/combobulate (by the same author).
(setq treesit-language-source-alist
      '(
        ;; Problems, don't compile:
        ; (bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.20.0")
        ; (jsx        "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src")
        ; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        ; (yaml "https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0")
        ;; Not using:
        ; (cmake "https://github.com/uyha/tree-sitter-cmake")
        ; (make "https://github.com/alemuller/tree-sitter-make")
        (css        "https://github.com/tree-sitter/tree-sitter-css"        "v0.20.0")
        (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
        (go         "https://github.com/tree-sitter/tree-sitter-go"         "v0.20.0")
        (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
        (html       "https://github.com/tree-sitter/tree-sitter-html"       "v0.20.1")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src")
        (json       "https://github.com/tree-sitter/tree-sitter-json"       "v0.20.2")
        (python     "https://github.com/tree-sitter/tree-sitter-python"     "v0.20.4")
        (toml       "https://github.com/tree-sitter/tree-sitter-toml"       "v0.5.1")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
        )
      )

;; (use-package treesit
;;   :mode (("\\.tsx\\'" . tsx-ts-mode)
;;          ("\\.js\\'"  . typescript-ts-mode)
;;          ("\\.mjs\\'" . typescript-ts-mode)
;;          ("\\.mts\\'" . typescript-ts-mode)
;;          ("\\.cjs\\'" . typescript-ts-mode)
;;          ("\\.ts\\'"  . typescript-ts-mode)
;;          ("\\.jsx\\'" . tsx-ts-mode)
;;          ("\\.json\\'" .  json-ts-mode)
;;          ("\\.Dockerfile\\'" . dockerfile-ts-mode)
;;          ("\\.prisma\\'" . prisma-ts-mode)))


;; ======================================================================

;; 7. Load custom packages.

(use-package emacs
  :custom

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  )


;; (a) Corfu for completion (alternative to company -- see old company config below).
(use-package corfu
  :ensure t

  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-auto-delay 2)

  (corfu-terminal-mode +1)

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)

  :hook (prog-mode . corfu-mode))


(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

(setq nerd-icons-corfu-mapping
      '(
        (array :style "cod" :icon "symbol_array" :face font-lock-type-face)
        (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
        ;; You can alternatively specify a function to perform the mapping,
        ;; use this when knowing the exact completion candidate is important.
        (file :fn nerd-icons-icon-for-file :face font-lock-string-face)
        ;; ...
        (t :style "cod" :icon "code" :face font-lock-warning-face)
        )
      )


;; Enable auto completion and configure quitting
(setq corfu-auto t
      corfu-quit-no-match 'separator)

(use-package nerd-icons
  :ensure t
  :after corfu
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package nerd-icons-completion
  :ensure t
  :after corfu
  :config
  (nerd-icons-completion-mode))

;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom
;;   (kind-icon-blend-background t)
;;   (kind-icon-default-face 'corfu-default) ; only needed with blend-background
;;   :config
;;   ;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
;;   ;; (setf kind-icon-use-icons t)
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
;;   )


;; Old config for complete-anything (company).
;; Kudos https://github.com/martenlienen/dotfiles/blob/e4f7c47/home/.emacs.d/straight/repos/mlextras/ml-init.org
;;
;; (use-package company
;;   :init
;;   (global-company-mode)
;;   (setf
;;    company-idle-delay 0
;;    company-minimum-prefix-length 2
;;    company-show-quick-access t
;;    company-selection-wrap-around t))


;; ·····························································
;; Currently, not using sideline.
;; The below config is just a reminder.
;; ·····························································

;; ;; (b) Sideline to display messages from Flymake.
;;
;; (use-package sideline-flymake)
;; (use-package sideline
;;   :hook
;;   (flymake-mode-hook . sideline-mode)
;;   :init
;;   (global-sideline-mode)
;;   (setq
;;    sideline-backends-right '(sideline-flymake)
;;    sideline-flymake-display-errors-whole-line 'line ; or 'point to show errors only on point
;;    ;; sideline-backends-skip-current-line t         ; don't display on current line
;;    sideline-order-right 'down                       ; or 'up
;;    sideline-format-left "%s   "                     ; format for left aligment
;;    sideline-format-right "   %s"                    ; format for right aligment
;;    sideline-priority 100                            ; overlays' priority
;;    sideline-display-backend-name 'f                 ; display the backend name
;;    )
;;   )
;;
;; ;; (c) Flymake bits and bobs.
;;
;; (use-package flymake-css)

;; (d) Misc.

; (use-package web-mode :mode "\\.html\\.?")
; (use-package js2-mode)
; (use-package json-mode)
; (use-package multi-web-mode)
(use-package markdown-mode)
; (use-package yaml-mode)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (setf rainbow-delimiters-max-face-count 6))

(use-package undo-tree
  :init
  (global-undo-tree-mode))

(setq undo-tree-auto-save-history t)

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; ======================================================================

;; 8. Eglot config.

(use-package eglot)

;; Tell Eglot which language servers are available.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(go-ts-mode         . ("gopls" "serve")))
  ;; (add-to-list 'eglot-server-programs '(go-ts-mode         . ("gopls" "-remote=:37374" "-logfile=auto" "-debug=:0" "-rpc.trace")))
  (add-to-list 'eglot-server-programs '(js-ts-mode         . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(tsx-ts-mode        . ("typescript-language-server" "--stdio")))
  ;; (add-to-list 'eglot-server-programs '(json-ts-mode      . ("typescript-language-server" "--stdio")))
  ;; (add-to-list 'eglot-server-programs '(python-ts-mode    . ("pylsp")))
  ;; (add-to-list 'eglot-server-programs '(sh-mode           . ("bash-language-server" "start")))
  ;; (add-to-list 'eglot-server-programs '(shell-script-mode . ("bash-language-server" "start")))

  ;; Thanks claude.ai for the following!
  ;; Set workspace configuration
  (add-to-list 'eglot-workspace-configuration
               '(:gopls
				 (
				  :usePlaceholders t
				  :staticcheck t
				  :completeUnimported t
				  )
				 )
			   )
  )

;; Enable Eglot generally...
;; ;; (add-hook 'prog-mode-hook 'eglot-ensure)

;; ...or enable individually:
(setq-default eglot-extend-to-xref t)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
;; (add-hook 'sh-mode-hook 'eglot-ensure)
;; (add-hook 'shell-script-mode-hook 'eglot-ensure)
;; (add-hook 'python-ts-mode-hook 'eglot-ensure)
;; etc.


;; From https://tip.golang.org/gopls/editor/emacs
;; Use built-in project package to identify the LSP workspace for a newly-opened buffer.
;; (require 'project)
;; (defun project-find-go-module (dir)
;;   (when-let ((root (locate-dominating-file dir "go.mod")))
;;     (cons 'go-module root)))
;; (cl-defmethod project-root ((project (head go-module)))
;;   (cdr project))
;; (add-hook 'project-find-functions #'project-find-go-module)


;; Keep eglot away from some stuff:
; (add-to-list 'eglot-stay-out-of "company")

;; Use Flymake to show messages from Eglot.
(add-hook 'eglot-managed-mode-hook #'flymake-mode)


;; FIXME -- the following isn't working :/
;; (require 'cl-lib)
;; (defun my-flymake-show-diagnostics-on-save ()
;;   "Show Flymake diagnostics buffer on save if errors exist."
;;   (interactive)
;;   (when (cl-some (lambda (diag)
;;                    (eq (flymake-diagnostic-type diag) :error))
;;                  (flymake-diagnostics))
;; 	(flymake-show-buffer-diagnostics))
;;   )
;; (add-hook 'eglot-managed-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook #'my-flymake-show-diagnostics-on-save nil t)))


;; ======================================================================

;; ;; 9. Eldoc config.
;;
;; ;; Prevent eldoc expanding the echo area.
;; ;; (setq eldoc-echo-area-use-multiline-p nil)
;;
;; (use-package eldoc
;;   :config
;;   (setf eldoc-idle-delay 0.2)
;;   (setq eldoc-echo-area-use-multiline-p nil))

;; ======================================================================

;; 10. Golang config.

;; Automatically format buffer on save using eglot.
;; Eglot uses the canonical Go formatter gofmt.
(add-hook 'go-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'eglot-format-buffer -10 t)))

;; Automatically update imports on save using goimports.
(add-hook 'before-save-hook
		  (lambda ()
			(call-interactively 'eglot-code-action-organize-imports))
		  nil t)


;; ======================================================================

;; 11. Miscellaneous.

;; Kudos https://github.com/martenlienen/dotfiles/blob/e4f7c47/home/.emacs.d/straight/repos/mlextras/ml-init.org

;; Make Escape quit prompts and act like C-g
(define-key key-translation-map (kbd "<escape>") (kbd "C-g"))

;; Table navigation in markdown tables
;; (add-hook 'markdown-mode-hook
;;           (lambda ()
;;             (local-set-key "M-<up>" 'table-backward-cell)
;;             (local-set-key "M-<down>" 'table-forward-cell)))

;; Show current function in status line.
(which-function-mode 1)

;; Line breaks at 88 characters, default for black.
(setq-default fill-column 88)

;; Single space after a full-stop.
(setq-default sentence-end-double-space nil)

;; Append a newline at the end of files
(setq require-final-newline t)

;; Remove trailing whitespace. Kudos https://github.com/fxbois/web-mode/issues/283#issuecomment-47773112
(add-hook 'local-write-file-hooks (lambda ()
				    (delete-trailing-whitespace) nil))

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
(setq backup-by-copying t               ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

					; use versioned backups

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

(setq-default indent-tabs-mode t)

(defun infer-indentation-style () ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min)
			       (point-max)))
	(tab-count (how-many "^\t" (point-min)
			     (point-max))))
    (if (> space-count tab-count)
	(setq indent-tabs-mode nil))
    (if (> tab-count space-count)
	(setq indent-tabs-mode t))))

;; Copy to X clipboard using M-c (left alt c).
(defun copy-selected (beg end)
  (interactive "*r")
  (if (region-active-p)
      (shell-command-on-region beg end "xsel -b")
    (message "No selection!")))

(global-set-key (kbd "M-c") 'copy-selected)

;; Hide/show menu bar using F10.
(menu-bar-mode -1)

					; start hidden
(global-set-key [f10] 'toggle-menu-bar-mode-from-frame)

;; Multiple cursors.
;; (global-set-key (kbd "C-M-j") 'mc/mark-all-dwim)

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

;; ;; Prettier for JavaScript.
;; (require 'prettier-js)
;; (add-hook 'js-mode-hook 'prettier-js-mode)
;; (add-hook 'js2-mode-hook 'prettier-js-mode)
;; (add-hook 'multi-web-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)
;;
;; (setq prettier-js-args
;;       '("--use-tabs"))
;; ;; ;; Biome for JavaScript formatting.
;; ;; (require 'biomejs-format)
;; ;; (add-hook 'js-mode-hook 'biomejs-format-mode)
;; ;; (add-hook 'js2-mode-hook 'biomejs-format-mode)
;; ;; ;; (add-hook 'web-mode-hook 'biomejs-format-mode)
;; ;; ;; (add-hook 'multi-web-mode-hook 'biomejs-format-mode)
;; ;; (setq biomejs-format-biome-args
;; ;;       '(
;; ;;         "format"
;; ;;         "--trailing-comma" "all"
;; ;;         "--bracket-spacing" "true"
;; ;;         "--javascript-formatter-indent-width" "4"
;; ;;         "--indent-style" "space"
;; ;;         )
;; ;;       )

;; ======================================================================

(defun d2-format-file ()
  "Format the current buffer file using the external 'd2 fmt' command, then reload."
  (interactive)
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (save-buffer)
    (shell-command (concat "d2 fmt " (shell-quote-argument buffer-file-name)))
    (revert-buffer :ignore-auto :noconfirm)))

;; (add-hook 'd2-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook #'d2-format-file nil t)))
