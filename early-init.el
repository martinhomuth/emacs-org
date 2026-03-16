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

;; Delay garbage collection while Emacs is booting
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Schedule garbage collection sensible defaults for after booting
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

;; Single VC backend inscreases booting speed
(setq vc-handled-backends '(Git))

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

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
