(custom-set-variables
 '(go-ts-mode-indent-offset 4)
 '(indent-tabs-mode t)
 '(tab-width 4)
 '(package-selected-packages '(eglot kind-icon treesit lsp-mode))
 )

(custom-set-faces
 '(font-lock-builtin-face ((((type tty)) (:foreground "royalblue3"))))
 '(font-lock-function-name-face ((t (:foreground "green"))))
 '(font-lock-keyword-face ((((type tty)) (:foreground "green4"))))
 '(font-lock-string-face ((((type tty)) (:foreground "purple"))))
 '(markdown-comment-face ((t (:inherit font-lock-comment-face :foreground "coral1"))))
 '(markdown-markup-face ((t (:inherit shadow :foreground "royalblue1" :slant normal :weight normal))))
 '(minibuffer-prompt ((((type tty)) (:foreground nil))))
 '(mode-line-buffer-id ((t (:foreground nil :weight bold))))
 )

;; (package-initialize)

(setq package-enable-at-startup nil)
(require 'package)

;; (require 'exec-path-from-shell)
;; (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG"
;; 	       "LANGUAGE" "LC_CTYPE" "PATH" "NODE_PATH"))
;; (add-to-list 'exec-path-from-shell-variables var))

(with-eval-after-load 'eglot
  ;; (add-to-list 'eglot-server-programs
  ;; 			   '(go-ts-mode . ("gopls" "serve")))
  (add-to-list 'eglot-server-programs
			   '(go-ts-mode . ("gopls" "-remote=:37374" "-logfile=auto" "-debug=:0" "-rpc.trace")))
)
(setq-default eglot-extend-to-xref t)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-to-list 'auto-mode-alist '("\\.go"    . go-ts-mode))
(add-to-list 'auto-mode-alist '("go.mod"   . go-mod-ts-mode))
(setq treesit-language-source-alist
      '(
        (go         "https://github.com/tree-sitter/tree-sitter-go"         "v0.20.0")
        (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
		)
      )
(add-hook 'eglot-managed-mode-hook #'flymake-mode)
(add-hook 'go-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)))
(add-hook 'before-save-hook
		  (lambda ()
			(call-interactively 'eglot-code-action-organize-imports))
		  nil t)


;; --------------------------------------------------------------------------------

(setq-default sentence-end-double-space nil)
(setq require-final-newline t)
(setf x-select-enable-clipboard t
      x-select-enable-primary t)
(setf initial-scratch-message "")
(column-number-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq scroll-conservatively 101)
(setq scroll-margin 3)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(setq backup-by-copying t               ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t                 ; use versioned backups
      )

(setq backup-directory-alist '(("." . "~/.local/tmp/")))
(setq auto-save-file-name-transforms `((".*" ,"~/.local/tmp/" t)))

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
(global-set-key [f10] 'toggle-menu-bar-mode-from-frame)


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
;; (my-terminal-config)
;; (add-hook 'after-make-frame-functions 'my-terminal-config)
