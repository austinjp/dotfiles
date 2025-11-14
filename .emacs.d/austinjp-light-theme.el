(deftheme austinjp-light
  "Created 2025-11-14.")

(custom-theme-set-variables
 'austinjp-light
 '(go-ts-mode-indent-offset 4)
 '(indent-tabs-mode t)
 '(package-selected-packages '(flatbuffers-mode jq-mode jq-ts-mode web-mode multi-web-mode nginx-mode d2-mode terraform-mode yaml-mode eglot nerd-icons-completion nerd-icons-corfu corfu-terminal kind-icon corfu quelpa treesit-fold treesit protobuf-mode basic-mode exec-path-from-shell fold-this gnu-elpa-keyring-update markdown-mode noxml-fold rainbow-delimiters undo-tree))
 '(tab-width 4)
 '(undo-limit 10000)
 '(undo-tree-limit 10000)
 '(undo-tree-outer-limit 10000)
 '(undo-tree-strong-limit 10000)
 '(ispell-dictionary nil))

(custom-theme-set-faces
 'austinjp-light
 '(font-lock-builtin-face ((((type tty)) (:foreground "royalblue3"))))
 '(font-lock-function-name-face ((t (:foreground "green"))))
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

(provide-theme 'austinjp-light)
