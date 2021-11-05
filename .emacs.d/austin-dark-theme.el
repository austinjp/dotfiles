(deftheme austin-dark
  "Created 2020-02-09.")

(custom-theme-set-variables
 'austin-dark
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(custom-safe-themes (quote ("fa2af0c40576f3bde32290d7f4e7aa865eb6bf7ebe31eb9e37c32aa6f4ae8d10" "6096a2f93610f29bf0f6fe34307587edd21edec95073cbfcfb9d7a3b9206b399" default)))
 '(package-selected-packages (quote (jedi elpy leuven-theme edit-indirect gnu-elpa-keyring-update)))
 '(pyvenv-virtualenvwrapper-python "/usr/bin/python3")
 '(pyvenv-mode t))

(custom-theme-set-faces
 'austin-dark
 '(highlight-indentation-face ((t nil)))
 '(table-cell ((t (:background "color-233" :foreground "brightwhite" :inverse-video nil)))))

(provide-theme 'austin-dark)
