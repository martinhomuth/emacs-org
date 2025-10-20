(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("mh-lisp" "mh-modules"))

(setq user-full-name "Martin Homuth")

(let ((file (expand-file-name "~/.emacs.d/secrets.el")))
  (if (file-exists-p file)
      (load file)))

;;; mail address based on username
(setq user-mail-address
      (let ((username (getenv "USER")))
        (cond ((string= username "martin") "martin@followthestack.tech")
              ((string= username "mhomuth") "mh@emlix.com")
              "martin@followthestack.tech")))

(require 'mh-org)

;; TODO: add default font and fontsize
(defvar mh-fontsize 12)
(add-to-list 'default-frame-alist
             `(font . ,(concat "Iosevka-" (number-to-string mh-fontsize))))

;; Treat clipboard as UTF-8 string first
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; see https://github.com/rolandwalker/unicode-fonts
(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-reuse-mode-window
         display-buffer-same-window
         display-buffer-in-previous-window)
        . ((mode . (org-mode helpful-mode help-mode)))))

(if (not (assoc "xterm-256color" term-file-aliases))
    (setq term-file-aliases (cons '("xterm-256color" . "rxvt")
                                  term-file-aliases)))
(if (not (assoc "st" term-file-aliases))
    (setq term-file-aliases (cons '("st" . "xterm-256color")
                                  term-file-aliases)))

(unless (package-installed-p 'gruvbox-theme)
  (package-refresh-contents)
  (package-install 'gruvbox-theme))
(load-theme 'gruvbox t)
(setq-default cursor-type 'box)

(if (or (not (fboundp 'server-running-p))
        (not (server-running-p)))
    (server-start))

(global-auto-revert-mode)

(use-package auto-compile
  :ensure t
  :init (setq load-prefer-newer t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(global-set-key (kbd "C-c T w") 'whitespace-mode)
(setq whitespace-line-column nil
      whitespace-display-mappings '((space-mark 32 [183] [46])
                                    (newline-mark 10 [9166 10])
                                    (tab-mark 9 [9654 9] [92 9])))

(use-package counsel
  :ensure t)
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "(%d/%d) ")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order)))
  (setq swiper-stay-on-quit t)
  :bind (("C-s" . 'swiper)
         ("C-r" . 'swiper)
         ("M-x" . 'counsel-M-x)
         ("C-x C-f" . 'counsel-find-file)
         ("M-y" . 'counsel-yank-pop)
         ("C-h a" . 'counsel-apropos)
         ("C-h v" . 'counsel-describe-variable)
         ("C-h f" . 'counsel-describe-function)
         ("C-h u" . 'counsel-unicode-char)
         ("C-x b" . 'counsel-switch-buffer)
         ("C-c j" . 'counsel-git-grep)
         ("C-c L" . 'counsel-git-log)
         ))
(use-package ivy-hydra
  :ensure t
  :after (ivy hydra))

(use-package hydra
  :ensure ace-window
  :ensure hydra
  :init
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))
  (global-set-key
   (kbd "C-M-o")
   (defhydra hydra-window ()
     "window"
     ("v" (\lambda ()
           (interactive)
           (split-window-right)
           (other-window 1))
      "vert")
     ("x" (\lambda ()
           (interactive)
           (split-window-below)
           (other-window 1))
      "horz")
     ("t" transpose-frame "'")
     ("o" delete-other-windows "one" :color blue)
     ("a" ace-window "ace")
     ("s" ace-swap-window "swap")
     ("d" ace-delete-window "del")
     ("i" ace-maximize-window "ace-one" :color blue)
     ("b" switch-to-buffer "buf")
     ("m" headlong-bookmark-jump "bmk")
     ("q" nil "cancel"))))

(defun martin-save-buffers-kill-emacs-with-confirm ()
  "Thanks to jsled for this method"
  (interactive)
  (if (window-system)
      (if (null current-prefix-arg)
	  (if (y-or-n-p "Are you sure you want to quit?")
	      (save-buffers-kill-emacs))
	(save-buffers-kill-emacs))
    (save-buffers-kill-terminal)))
(global-set-key "\C-x\C-c" 'martin-save-buffers-kill-emacs-with-confirm)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode ;; used to remove mode line information that is not used
  :init (yas-global-mode)
  :config
  (progn
    (yas-global-mode)
    (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (setq yas-key-syntaxes '("w_" "w_." "^ "))
    (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
    (setq yas-expand-only-for-last-commands nil)
    (yas-global-mode 1)
    (bind-key "\t" 'hippie-expand yas-minor-mode-map)))

(setq default-cursor-color "gray")
(setq yasnippet-can-fire-cursor-color "purple")

;; It will test whether it can expand, if yes, cursor color -> green.
(defun yasnippet-can-fire-p (&optional field)
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas-expand-only-for-last-commands
                 (not (member last-command yas-expand-only-for-last-commands)))
      (setq templates-and-pos (if field
                                  (save-restriction
                                    (narrow-to-region (yas--field-start field)
						      (yas--field-end field))
                                    (yas--templates-for-key-at-point))
                                (yas--templates-for-key-at-point))))
    (and templates-and-pos (first templates-and-pos))))

(defun my/change-cursor-color-when-can-expand (&optional field)
  (interactive)
  (when (eq last-command 'self-insert-command)
    (set-cursor-color (if (my/can-expand)
                          yasnippet-can-fire-cursor-color
                        default-cursor-color))))

(defun my/can-expand ()
  "Return true if right after an expandable thing."
  (or (abbrev--before-point) (yasnippet-can-fire-p)))

(add-hook 'post-command-hook 'my/change-cursor-color-when-can-expand)

(defun my/insert-space-or-expand ()
  "For binding to the SPC SPC keychord."
  (interactive)
  (condition-case nil (or (my/hippie-expand-maybe nil) (insert "  "))))

(defun mh__current-calendar-week ()
  "Return the current ISO 8601 calendar week number as an integer."
  (let* ((time (current-time))
         (decoded (decode-time time))
         (day (nth 3 decoded))
         (month (nth 4 decoded))
         (year (nth 5 decoded)))
    (car (calendar-iso-from-absolute
          (calendar-absolute-from-gregorian (list month day year))))))

(defun mh__update-timeclock-file-daily()
    "Updates the variable `timeclock-file` once per day"
    (setq timeclock-file (concat "~/nextcloud-work/timelog/cw" (number-to-string (mh__current-calendar-week)))))

(use-package timeclock
  :ensure t
  :init
  (display-time-mode)
  (timeclock-mode-line-display)
  (mh__update-timeclock-file-daily)
  (run-at-time "1 hour" 3600 #'mh__update-timeclock-file-daily)
  :config
  (define-key ctl-x-map "ti" 'timeclock-in)
  (define-key ctl-x-map "to" 'timeclock-out)
  (define-key ctl-x-map "tc" 'timeclock-change)
  (define-key ctl-x-map "tr" 'timeclock-reread-log)
  (define-key ctl-x-map "tu" 'timeclock-update-mode-line)
  (define-key ctl-x-map "tw" 'timeclock-when-to-leave-string)
  (define-key ctl-x-map "tR" 'timeclock-generate-report)
  (add-hook 'kill-emacs-query-functions #'timeclock-query-out)
  (setq display-time-load-average nil
        timeclock-relative nil))

;; General
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-S-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-S-s") 'isearch-forward-regexp)
(global-unset-key (kbd "C-z")) ;; who needs that anyways?
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'mh-prev-other-window)
(global-set-key (kbd "M-SPC") 'mark-sexp)
(global-unset-key "\C-xf")
(global-set-key [f1] 'eshell)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x b") 'magit-blame-echo)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-x r l") 'counsel-bookmark)
(global-set-key (kbd "<f1>") 'vterm)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(custom-set-faces
 '(ediff-current-diff-A ((t (:foreground "Black"))))
 '(ediff-fine-diff-A ((t (:foreground "Black"))))
 '(ediff-current-diff-B ((t (:foreground "Black"))))
 '(ediff-fine-diff-B ((t (:foreground "Black"))))
 '(ediff-current-diff-C ((t (:foreground "Black"))))
 '(ediff-fine-diff-C ((t (:foreground "Black"))))
;;; add font customization for info entries
 '(Info-quoted ((t (:slant oblique :weight bold)))))

(global-set-key (kbd "C-x p i") 'mh-pomodoro-start-focus)
(global-set-key (kbd "C-x p b") 'mh-pomodoro-start-break)
(global-set-key (kbd "C-x p o") 'mh-pomodoro-stop)
(global-set-key (kbd "C-x p r") 'mh-pomodoro-remaining-time)

(require 'notifications)
(defun mh-pomodoro-start-focus()
  """ Starts a focus period """
  (interactive)
  (let ((focus-period 25))
    (notifications-notify
     :title "Focus period started"
     :on-action 'mh-pomodoro-start-focus
     :timeout 1500
     )

    (org-timer-set-timer focus-period)))

(defun mh-pomodoro-start-break()
  """ Starts a break period """
  (interactive)
  (let ((break-period 5))
    (notifications-notify
     :title "Break period started"
     :on-action 'mh-pomodoro-start-focus
     :timeout 1500
     )
    (org-timer-set-timer break-period)))

(defun mh-pomodoro-stop()
  """ Stops the pomodoro timer """
  (interactive)
  (notifications-notify
   :title "Pomodoro Timer stopped"
   :on-action 'mh-pomodoro-start-focus
   :timeout 1500
   )
  (org-timer-stop))

(setq org-clock-sound "~/Nextcloud/Martin/bell.wav")

(defun mh-pomodoro-remaining-time()
  """ Reports the remaining time """
  (interactive)
  (let ((remaining-time (org-timer-show-remaining-time)))
    (notifications-notify
     :title "Remaining time"
     :body remaining-time
     :timeout 1500
     )))

(setopt scroll-step 1
	scroll-margin 5
	scroll-conservatively 101)

(setq sentence-end-double-space nil)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
(global-set-key [remap org-beginning-of-line]  'smarter-move-beginning-of-line)

(defun mh-prev-other-window()
  "Simple function wrapper to `other-window' with a negative argument"
  (interactive)
  (other-window -1))

(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (if buffer-file-name
			 (read-file-name "Move file to: ")
		       (read-file-name "Move file to: "
				       default-directory
				       (expand-file-name (file-name-nondirectory (buffer-name))
							 default-directory)))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (buffer-file-name)))
    (write-file new-location t)
    (when (and old-location
	       (file-exists-p new-location)
	       (not (string-equal old-location new-location)))
      (delete-file old-location))))

(bind-key "C-x C-m" 'move-file)

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" .  yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )

(use-package company
  :ensure t
  :init
  (global-company-mode))

(use-package expand-region
  :ensure t
  :commands ( er/expand-region er/contract-region )
  :init
  (global-set-key (kbd "M-r") #'er/expand-region)
  (global-set-key (kbd "M-q") #'er/contract-region)
  )

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

(setq eshell-cmpl-compare-entry-function (quote string-lessp))

(add-to-list 'load-path "~/git/bb-mode")

(require 'bb-mode)
(setq auto-mode-alist (cons '("\\.bb$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.inc$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbappend$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbclass$" . bb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.conf$" . bb-mode) auto-mode-alist))

(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'c-mode-hook 'turn-on-auto-fill)
(add-hook 'TeX-mode-hook 'turn-on-auto-fill)

;;; It is the opposite of fill-paragraph    
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

(use-package magit
  :ensure t
  :init
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0"))

(setq vc-follow-symlinks t)

(use-package projectile
  :ensure t
  :bind (
         ("C-c p p" . projectile-switch-project)
         ("C-c p r" . projectile-ripgrep)
         ("C-c p b" . projectile-switch-to-buffer)
         ("C-c p f" . projectile-find-file)
         )

  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t
        projectile-switch-project-action 'projectile-dired
        )

  )

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))
(setq mail-header-separator "")
(add-hook 'message-mode-hook
	  'turn-on-auto-fill
	  (function
	   (lambda ()
	     (progn
	       (local-unset-key "\C-c\C-c")
	       (define-key message-mode-map "\C-c\C-c" '(lambda ()
							  "save and exit quickly"
							  (interactive)
							  (save-buffer)))))))

(add-hook 'message-mode-hook 'turn-on-orgtbl)

(when (executable-find "notmuch")
  (define-key global-map "\C-cm" 'notmuch)
  (setq sendmail-program "/usr/bin/msmtp"
        notmuch-search-oldest-first nil
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-specify-envelope-from 'header
        notmuch-show-all-multipart/alternative-parts nil
        notmuch-fcc-dirs "emlix/Sent +sent -unread"
        ))

(add-hook 'notmuch-hello-refresh-hook
	  (lambda ()
            (if (and (eq (point) (point-min))
                     (search-forward "Saved searches:" nil t))
                (progn
		  (forward-line)
		  (widget-forward 1))
              (if (eq (widget-type (widget-at)) 'editable-field)
		  (beginning-of-line)))))

(setq notmuch-crypto-process-mime t)

(setq notmuch-search-line-faces '(("unread" :weight bold)
                                  ("flagged" :foreground "red")))

(setq martin/notmuch-activity-string "")
(add-to-list 'global-mode-string '((:eval martin/notmuch-activity-string)) t)
(defun martin/get-notmuch-incoming-count ()
  (string-trim
   (shell-command-to-string
    "notmuch count tag:inbox AND tag:unread AND '\(folder:INBOX or folder:INBOX.Eyeo\)'")))
(defun martin/format-notmuch-mode-string (count)
  (concat " mails[" (if (string= count "0") "" count) "]"))
(defun martin/update-notmuch-activity-string (&rest args)
  (setq martin/notmuch-activity-string
        (martin/format-notmuch-mode-string (martin/get-notmuch-incoming-count)))
  (force-mode-line-update))
(add-hook 'notmuch-after-tag-hook 'martin/update-notmuch-activity-string)
(defcustom notmuch-presync-hook nil
  "Hook run before notmuch is synchronised"
  :type 'hook)
(defcustom notmuch-postsync-hook '(martin/update-notmuch-activity-string)
  "Hook run after notmuch has been synchronised"
  :type 'hook)

(use-package prog-mode
  :config
  (define-key prog-mode-map (kbd "M-q") nil))

(setq lsp-use-plists t)
(use-package ccls
  :ensure t
  :init
  (setq lsp-lens-enable nil)
  :config
  (setq ccls-executable "/usr/bin/ccls"
        gc-cons-threshold (* 1024 1024 100)
        read-process-output-max (* 1024 1024) ;; 1mb
        lsp-enable-on-type-formatting nil)
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp)))
  )
(use-package lsp-mode
  :ensure t
  :commands lsp
  )
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  )

(use-package cc-mode
  :config
  (define-key c-mode-base-map (kbd "M-q") nil))

(defun c-lineup-arglist-tabs-only ()
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun my/general-c-mode-configuration ()
  (setq c-basic-offset 8
        cdefault-style "linux"
        tab-width 8
        indent-tabs-mode nil
        c-set-style "linux-tabs-only")
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))

(defun my/general-c++-mode-configuration ()
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))

(add-hook 'c-mode-common-hook 'my/general-c-mode-configuration)
(add-hook 'c++-mode-hook 'my/general-c++-mode-configuration)
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(use-package flycheck
  :ensure flycheck-cstyle
  :config
  (eval-after-load 'flycheck
    '(progn
       (flycheck-cstyle-setup)
       (flycheck-add-next-checker 'c/c++-cppcheck '(warning . cstyle))))
  (global-flycheck-mode)
  (add-hook 'c-mode-hook
            (lambda () (setq flycheck-gcc-include-path
                             (list "/usr/src/linux/include" ))))
  (add-hook 'c-mode-hook
            (lambda () (setq flycheck-gcc-language-standard "c11")))
  (add-hook 'cc-mode-hook
            (lambda () (setq flycheck-gcc-language-standard "c++17")))
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(defun my-highlight-keywords-warning()
  ""
  (font-lock-add-keywords nil
			  '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\|TBD\\):"
			     1 font-lock-warning-face prepend))))
(defun my-highlight-keywords-info()
  ""
  (font-lock-add-keywords nil
			  '(("\\<\\(NOTE\\|INFO\\):"
			     1 font-lock-comment-face prepend))))

(add-hook 'c-mode-hook 'my-highlight-keywords-warning)
(add-hook 'c-mode-hook 'my-highlight-keywords-info)
(add-hook 'c++-mode-hook 'my-highlight-keywords-warning)
(add-hook 'c++-mode-hook 'my-highlight-keywords-info)

(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c C-l") 'compile)))

(defun martin-setup-sh-mode()
  "sh-mode and shell-mode customizations."
  (interactive)
  (lambda()
    (setq indent-tabs-mode nil)
    (setq tab-width 4)))

(add-hook 'sh-mode-hook 'martin-setup-sh-mode)
(add-hook 'shell-mode-hook 'martin-setup-sh-mode)

(add-hook 'sh-mode-hook 'flycheck-mode)

(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
	       '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

(use-package dts-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (defun my-setup-php ()
    ;; enable web mode
    (web-mode)

    ;; make these variables local
    (make-local-variable 'web-mode-code-indent-offset)
    (make-local-variable 'web-mode-markup-indent-offset)
    (make-local-variable 'web-mode-css-indent-offset)

    ;; set indentation, can set different indentation level for different code type
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-markup-indent-offset 2))
  (add-to-list 'auto-mode-alist '("\\.php$" . my-setup-php))
  )

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(use-package js2-mode
  :ensure t
  :init
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-highlight-level 3))

(use-package ac-js2
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'ac-js2-mode))


(use-package flymake-jslint
  :ensure t
  :config
  (add-to-list 'load-path (file-truename "~/git/lintnode"))
  (setq lintnode-location (file-truename "~/git/lintnode"))
  (setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
    ;;; TODO: does not work currently, investigate
					; (add-hook 'js-mode-hook
					;	    (lambda()
					; (lintnode-hook))))
  )

(use-package slime
  :ensure t)

(setq inferior-lisp-program "/usr/bin/clisp")
