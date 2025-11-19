(deftheme austinjp-dark
  "Created 2025-11-14.")

(custom-theme-set-faces
 'austinjp-dark

 '(flymake-error ((t (:foreground "brightwhite"  :background "firebrick"))))

 '(font-lock-builtin-face ((t (:foreground "royalblue1"))))
 '(font-lock-comment-face ((t (:foreground "coral1"))))
 '(font-lock-constant-face ((t (:foreground "palevioletred1"))))
 '(font-lock-function-name-face ((t (:foreground "green"))))
 '(font-lock-keyword-face ((t (:foreground "mediumseagreen"))))
 '(font-lock-string-face ((t (:foreground "deepskyblue3"))))
 '(font-lock-variable-name-face ((t (:foreground "brightyellow"))))

 '(markdown-comment-face ((t (:inherit font-lock-comment-face :foreground "coral1"))))
 '(markdown-markup-face ((t (:inherit shadow :foreground "royalblue1" :slant normal :weight normal))))

 '(minibuffer-prompt ((t (:foreground "white"))))
 ;; '(mode-line-buffer-id ((t (:foreground "white" :weight bold))))

 '(rainbow-delimiters-base-face ((t (:inherit nil))))
 '(rainbow-delimiters-base-error-face ((t (:inherit rainbow-delimiters-base-face :foreground "brightred"))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "green"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan4"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-depth-1-face))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-depth-2-face))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-depth-3-face))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-depth-1-face))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-depth-2-face))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-depth-3-face))))
 '(rainbow-delimiters-mismatched-face ((t (:inherit rainbow-delimiters-unmatched-face))))

 '(region ((t (:extend t :background "goldenrod" :foreground "black"))))
 )

(provide-theme 'austinjp-dark)
