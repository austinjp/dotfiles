
;; Set TLS to use version 1.2 for elpa.gnu.org.
;; TLS version 1.1 also works, but 1.3 does NOT.
;; Note this must be BEFORE package-initialize.
;; May need to do: sudo apt install gnutls-bin libgnutls28-dev
;; Some background: https://emacs.stackexchange.com/a/51772
;; Check supported TLS versions here:
;; https://www.ssllabs.com/ssltest/analyze.html?d=elpa.gnu.org&hideResults=on
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; =====================================================================

;; See http://tapoueh.org/emacs/el-get.html
;; and https://github.com/dimitri/el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; (require 'el-get)
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/recipes")
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/")
(setq el-get-verbose t)

;; ;; personal recipes
;; (setq el-get-sources
;;       '((:name el-get :branch "master")
;; 
;;         ;; (:name magit
;;         ;;              :before (global-set-key (kbd "C-x C-z") 'magit-status))
;; 
;;         (:name expand-region
;;                       :before (global-set-key (kbd "C-@") 'er/expand-region))
;; 
;;         (:name descbinds-anything
;;                       :after (progn
;;                                (descbinds-anything-install)
;;                                (global-set-key (kbd "C-h b") 'descbinds-anything)))
;; 
;;         (:name goto-last-change
;;                       :before (global-set-key (kbd "C-x C-/") 'goto-last-change))))


;; =====================================================================

;; my packages
(setq dim-packages
      (append
       ;; list of packages we use straight from official recipes
       '(
         ;; better-defaults
         ;; gnus
         ;; bbdb
         ;; switch-window
         ;; vkill
         ;; google-maps
         ;; pgdevenv-el
         ;; mbsync
         ;; asciidoc
         ;; smex
         ;; geiser
         ;; xcscope
         ;; multiple-cursors
         ;; anything
         ;; descbinds-anything
         ;; pcmpl-git
         ;; magit-view-file
         ;; emacs-goodies-el
         ;; sicp
         ;; auto-dictionnary
         ;; keywiz
         pandoc-mode
         ;; pgsql-linum-format
         ;; psvn
         ;; rect-mark
         ;; crontab-mode
         ;; icomplete+
         ;; php-mode-improved
         ;; rainbow-delimiters
         ;; muse
         ;; deft
         ;; dpans2texi
         markdown-mode
         color-theme-solarized
         ;; multi-mode
         ;; protobuf-mode
         ;; paredit
        )

       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync dim-packages)

;; =====================================================================

;; Unset an annoying key-binding that comes from markdown-mode
;; but which does something weird and destroys the buffer contents.

;; (global-set-key (kbd "S-<tab>") nil)
;; (global-unset-key (kbd "S-<tab>"))
;; (global-set-key (kbd "S-<iso-lefttab>") nil)
;; (global-unset-key (kbd "S-<iso-lefttab>"))


;; =====================================================================

;; Package managers.

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))

  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/"))))

  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;; (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" "http://melpa.org/packages/") t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)

  ;; Org mode
  ;; (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)

  ;; Elpy repository
  (add-to-list 'package-archives (cons "elpy" (concat proto "://jorgenschaefer.github.io/packages/")) t)

  ;; Milkbox.net no longer hosts this:
  ;; (add-to-list 'package-archives ("melpamilkbox" . "https://melpa.milkbox.net/packages/") t)
)

;; =====================================================================

;; Update packages:
(package-initialize)

;; If there are no archived package contents, refresh them:
(when (not package-archive-contents) (package-refresh-contents))

;; =====================================================================

(menu-bar-mode -1)
(ido-mode -1)

(setq backup-by-copying t      ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 6
      version-control t)       ; use versioned backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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

;; Enable font lock for LaTeX maths expressions.
(setq-default markdown-enable-math t)

;; =====================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("6096a2f93610f29bf0f6fe34307587edd21edec95073cbfcfb9d7a3b9206b399" "86e410cba23455840ec0b55cde70eb04cecdc961569f1b4bf4d775738ce73ddb" "a156cf298f369349e34da18a6d1f353b83ac0539da78c8334938dcfb120f378c" "fa2af0c40576f3bde32290d7f4e7aa865eb6bf7ebe31eb9e37c32aa6f4ae8d10" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "f8e384517a35bd7430acb09e7925ab73c79507873e38b442eee2183887250b81" default)))
 '(display-line-numbers-type nil)
 '(global-display-line-numbers-mode t)
 '(load-home-init-file t t)
 '(package-selected-packages
   (quote
    (nim-mode tide typescript-mode svelte-mode cython-mode which-key web-mode flycheck-yamllint flycheck-pycheckers flycheck-popup-tip flycheck-mypy flycheck-languagetool flycheck-checkbashisms flycheck magit psgml dockerfile-mode markdown-mode spinner ejc-sql simple-httpd skewer-mode quote
              (jedi elpy leuven-theme edit-indirect gnu-elpa-keyring-update))))
 '(pyvenv-virtualenvwrapper-python "/usr/bin/python3")
 '(show-paren-mode t))

;; =====================================================================

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; =====================================================================

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; =====================================================================

;; Ensure table.el lives at ~/.emacs.d/table/table.el for this to work.
;; Edit tables in org-mode followed by C-c ' to edit in separate window.
(add-to-list 'load-path "~/.emacs.d/table/")
(require 'table)
;; Increase memory that tables can use:
(setq max-lisp-eval-depth '40000)
(setq max-specpdl-size '100000)

;; =====================================================================

;; Attempt to fix issues with long lines being slow.
;; See https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
;; However, the simplest solution appears to be to simply
;; drop into fundamental-mode as necessary.

;; (setq-default bidi-display-reordering nil)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

;; =====================================================================

;; elpy IDE
;; sudo apt install elpa-elpy
;; See https://elpy.readthedocs.io/en/latest/introduction.html#installation

(elpy-enable)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; =====================================================================

;; (desktop-save-mode 1) ;; Incompatible with perspective.
; (require 'perspective)
; (persp-mode)
; 
; (setq display-buffer-alist
;       '(("\\*compilation\\*"
;          (display-buffer-reuse-window display-buffer-same-window))
;         ;; default
;         (".*"
;          (display-buffer-same-window))))
; 
; (setq display-buffer-reuse-frames t)         ; reuse windows in other frames
; (setq pop-up-windows nil)                    ; display-buffer: avoid splitting
; (setq even-window-heights nil)               ; display-buffer: avoid resizing

;; =====================================================================

(require 'powerline)
(powerline-default-theme)

;; =====================================================================

; (set-face-background 'default nil (selected-frame))

(defun set-background-for-terminal (&optional frame)
  (or frame (setq frame (selected-frame)))
  "unsets the background color in terminal mode"
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions 'set-background-for-terminal)
(add-hook 'window-setup-hook 'set-background-for-terminal)

;; =====================================================================

;; Neotree. Nice.

(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


;; =====================================================================

;; Window-pane navigation:

;; (global-set-key (kbd "C-S-<left>")  'windmove-left)
;; (global-set-key (kbd "C-S-<right>") 'windmove-right)
;; (global-set-key (kbd "C-S-<up>")    'windmove-up)
;; (global-set-key (kbd "C-S-<down>")  'windmove-down)


;; =====================================================================

;; Key-bindings for window resizing.

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; =====================================================================

;; Emacs can use Chrome DevTools as a JavaScript "IDE". Sweet!

;; Skewer mode. To start: M-x skewer-run
;; (add-to-list 'load-path "~/.emacs.d/skewer-mode")
;; (require 'skewer-mode)

;; https://github.com/tungd/kite-mini.el
;; (add-to-list 'load-path "~/.emacs.d/kite-mini/")
;; (require 'kite-mini)

;; =====================================================================

;; Inspired by Prelude Emacs, a bunch of packages.

;; (require 'undo-tree)
(undo-tree-mode t)
;; (require 'magit)
;; (require 'web-mode)
;; (require 'which-key)
(which-key-mode t)

;; =====================================================================
;; Magit setup.

(setq magit-view-git-manual-method 'man)

;; =====================================================================
;; Flycheck setup.
;; https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea

;; Disable Flymake:
(setq flymake-start-on-flymake-mode nil)

;; ;; (use-package flycheck :ensure t :init (global-flycheck-mode t))
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))
;; ;; Disable default jshint:
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(javascript-jshint json-jsonlist)))
;; Enable eslint checker for web-mode
;; (flycheck-add-mode 'javascript-eslint 'web-mode)


;; =====================================================================
;; Web-mode setup.
;; Auto-enable for .js/.jsx files:
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
;; JSX syntax highlighting:
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(add-hook 'web-mode-hook  'web-mode-init-hook)
