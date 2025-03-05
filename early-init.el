(setq custom-file (expand-file-name "custom.el" (concat (getenv "HOME") "/.emacs.d/")))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path "~/elisp/org-mode/lisp")
(add-to-list 'load-path "~/elisp/org-mode/contrib/lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("mh-lisp" "mh-modules"))

(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; also keep the packages up to date
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(show-paren-mode 1)

(setq fill-column 120
      initial-scratch-message ""
      show-paren-delay 0
      show-paren-style 'mixed
      visible-bell t
      browse-url-browser-function 'browse-url-chromium
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

;; Whitespaces... A colleague of mine showed me the horror of not
;; considering whitespaces and thus they are shown always and everywhere!
(setq-default show-trailing-whitespace t)
