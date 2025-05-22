(use-package org
  :ensure t
  :init
  (setq org-log-done 'time)
  (setq org-clock-report-include-clocking-task t)
  :config
  (add-hook 'org-clock-in-hook 'martin/org-clock-in-set-state-to-started)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (setq org-hide-emphasis-markers t
        org-src-window-setup 'current-window
        org-support-shift-select t
        org-use-speed-commands t
	org-adapt-indentation t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (shell . t)
     (js . t)
     (python . t)
     (C . t)
     (css . t)
     (dot . t)
     (plantuml . t)
     (emacs-lisp . t)))
  )

(bind-key "C-c v" 'org-show-todo-tree org-mode-map)
(bind-key "C-c C-r" 'org-refile org-mode-map)
(bind-key "C-c R" 'org-reveal org-mode-map)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-ellipsis "➔"))

(use-package org-fancy-priorities
  :diminish
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))

;; Also here is `isamert/prettify-mode' macro.
;; You don't need this but it's a bit more convinient if you make use of
;; prettify-symbols minor mode a lot
(defmacro isamert/prettify-mode (mode pairs)
  "Prettify given PAIRS in given MODE. Just a simple wrapper around `prettify-symbols-mode`"
  `(add-hook ,mode (lambda ()
                     (mapc (lambda (pair)
                             (push pair prettify-symbols-alist))
                           ,pairs)
                     (prettify-symbols-mode))))

(isamert/prettify-mode 'org-mode-hook
                       '(("[ ]" . "☐")
                         ("[X]" . "☑" )
                         ("[-]" . "❍" )))

(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-directory (expand-file-name "~/git/org-files"))
(setq org-default-notes-file (expand-file-name "~/git/org-files/personal.org"))

(defvar martin/org-project-template "* %^{Project Description} %^g
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:10|0:15|0:30|0:45|2:00|4:00|8:00}
:END:
SCHEDULED: %^t
- Why?
  %?
- What?
- Who?
- Where?
- How?
- Outcome?
** Brainstorming
  Collect 10 Ideas
" "Full Project Description")
(defvar martin/org-basic-task-template "* TODO %^{Task}
SCHEDULED: %^t
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:10|0:15|0:30|0:45|2:00|4:00}
:END:
%?
" "Basic task data")
(defvar martin/org-programming-workout-template "* %^{Workout Description}
:PROPERTIES:
:Effort: %^{effort|0:05|0:10|0:15|0:20|0:25}
:END:
%^g%?
" "Programming Workout Template")
(defvar martin/org-journal-template
  "**** %^{Description} %^g
     %?"
  "Journal Template")
(defvar martin/org-work-journal-template
  "**** %^{Description} %^g
     %T
     %?"
  "Work Journal Template")
(setq org-capture-templates
      `(("t" "Tasks" entry
         (file+headline "~/git/org-files/personal.org" "INBOX")
         ,martin/org-basic-task-template)
        ("T" "Quick Task" entry
         (file+headline "~/git/org-files/personal.org" "INBOX")
         "* TODO %^{Task}"
         :immediate-finish t)
        ("j" "Journal entry" plain
         (file+olp+datetree "~/git/org-files/journal.org")
         ,martin/org-journal-template)
        ("a" "Appointments" entry
         (file+headline "~/git/org-files/organizer.org" "Appointments")
         "* %?\n%i")
        ("d" "Decisions" entry
         (file+headline "~/git/org-files/personal.org" "Decisions")
         "* %?\n%i")
        ("d" "Decisions" entry
         (file+headline "~/git/org-files/personal.org" "Decisions")
         "* %?\n%i")
        ("e" "Emlix journal entry" plain
         (file+olp+datetree "~/git/org-files/emlix-journal.org")
         ,martin/org-work-journal-template)
        ("p" "Project" entry
         (file+headline "~/git/org-files/personal.org" "Projects")
         ,martin/org-project-template)
        ("w" "Work journal entry" plain
         (file+olp+datetree "~/git/org-files/work-journal.org")
         ,martin/org-work-journal-template)
        ("W" "Workout" entry
         (file+headline "~/git/org-files/personal.org" "Primary Skills")
         ,martin/org-programming-workout-template)))
(bind-key "C-M-r" 'org-capture)

(setq org-reverse-note-order t)
(setq org-refile-use-outline-path nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-cache nil)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(setq org-todo-keywords
      '((sequence
         "TODO(t)"   ; next action
         "TOBLOG(b)"  ; next action
         "REVIEW(r)"  ; next action
         "STARTED(s)"
         "WAITING(w@/!)"
         "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
        (sequence "TODELEGATE(-)" "DELEGATED(d)" "|" "COMPLETE(x)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "green" :weight bold))
        ("DONE" . (:foreground "cyan" :weight bold))
        ("REVIEW" . (:foreground "blue" :weight bold))
        ("WAITING" . (:foreground "red" :weight bold))
        ("SOMEDAY" . (:foregound "gray" :weight bold))))

(setq org-tag-alist '(("call" . ?c)
                      ("@computer" . ?l)
                      ("@home" . ?h)
                      ("errand" . ?e)
                      ("@office" . ?o)
                      ("@anywhere" . ?a)
                      ("meetings" . ?m)
                      ("readreview" . ?r)
                      ("writing" . ?w)
                      ("programming" . ?p)
                      ("short" . ?s)
                      ("quantified" . ?q)
                      ("highenergy" . ?1)
                      ("lowenergy" . ?0)
                      ("business" . ?B)))

(setq my-org-agenda-files-list (append
                                (file-expand-wildcards "~/git/org-files/*.org"))
      org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    my-org-agenda-files-list)))

(setq org-agenda-span 'week)
(setq org-agenda-sticky nil)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-columns-default-format "%50ITEM %12SCHEDULED %TODO %3PRIORITY %Effort{:} %TAGS")

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

(use-package org-gtd
  :after org
  :ensure t
  :init
  (setq org-edna-use-inheritance t
        org-gtd-directory (file-truename "~/Nextcloud/private/org/gtd")
        org-gtd-clarify-show-horizons 'right
        org-gtd-areas-of-focus '("Home" "Health" "Family" "Career" "Beer")
        )
  (org-edna-mode 1)
  :bind (("C-c d c" . org-gtd-capture)
         ("C-c d e" . org-gtd-engage)
         ("C-c d f" . org-gtd-area-of-focus-set-on-item-at-point)
         ("C-c d l" . org-gtd-clarify-item)
         :map org-gtd-clarify-map
         ("C-c c" . org-gtd-organize)
         ("C-c f" . org-gtd-area-of-focus-set-on-item-at-point)
         ("C-c t" . org-gtd-clarify-toggle-horizons-window))
  )

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Nextcloud/Martin/RoamNotes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(provide 'mh-org)
