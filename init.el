(setq custom-file (expand-file-name "custom.el" (concat (getenv "HOME") "/.emacs.d/")))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path "~/elisp/org-mode/lisp")
(add-to-list 'load-path "~/elisp/org-mode/contrib/lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(setq package-enable-at-startup nil)
(package-initialize)
(require 'org)
(org-babel-load-file (concat (getenv "HOME") "/.emacs.d/" user-login-name ".org"))
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
