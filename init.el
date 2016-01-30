(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "d9a09bb02e2a1c54869dfd6a1412553fe5cb2d01a94ba25ef2be4634d1ca2c79" default)))
 '(default-input-method "german-prefix"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-initialize nil)

(add-to-list 'load-path "~/elisp/org-mode/lisp")
(add-to-list 'load-path "~/elisp/org-mode/contrib/lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(package-initialize t)
(setq package-enable-at-startup nil)
(org-babel-load-file "~/.emacs.d/ze0.org")
(put 'narrow-to-region 'disabled nil)
