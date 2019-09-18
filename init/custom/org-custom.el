(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-block-separator
   "════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════")
 '(org-agenda-custom-commands
   (quote
    ((":" . "List stub headings")
     (":s" "All headings tagged as stubs, except those with a STUB_TYPE_FLAG set to PHONY or IGNORE" tags "stub-STUB_TYPE_FLAG={PHONY\\|IGNORE}" nil)
     (":i" "All headings tagged with stub and INBOX, except those with a STUB_TYPE_FLAG set to PHONY or IGNORE" tags "INBOX+stub-STUB_TYPE_FLAG={PHONY\\|IGNORE}"
      ((org-agenda-overriding-header "Stubs in INBOX trees")))
     (":S" "All headings tagged as stubs, including those normally hidden by their STUB_TYPE_FLAG property" tags "stub" nil)
     (":t" "All active TODO headings tagged as stubs, except those with a STUB_TYPE_FLAG set to PHONY or IGNORE" tags-todo "stub-STUB_TYPE_FLAG={PHONY\\|IGNORE}"
      ((org-agenda-overriding-header "Stub TODO headings")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote todo)
          (quote done))))))
     (":T" "All TODO headings tagged as stubs, including DONE and ABORTED ones, and those normally hidden by their STUB_TYPE_FLAG property" tags-todo "stub" nil)
     (":!" "All headings with a STUB_TYPE_FLAG that aren't tagged with stub" tags "STUB_TYPE_FLAG={.+}-stub" nil)
     (";" . "Multi block agenda views")
     (";W" "Agenda, coupled with WORK tasks and WORK stubs"
      ((agenda "" nil)
       (todo "TODO|IN-PROGRESS|WAITING"
             ((org-agenda-overriding-header "Unscheduled TODO items tagged as WORK")
              (org-agenda-skip-function
               (quote
                (org-agenda-skip-entry-if
                 (quote scheduled))))))
       (todo "GOAL"
             ((org-agenda-overriding-header "All GOAL items tagged as WORK"))))
      ((org-agenda-tag-filter-preset
        (quote
         ("-CHORE" "+WORK")))
       (org-agenda-files
        (quote
         ("~/.orgfiles/work.org" "~/.orgfiles/general.org")))))
     (";;" "Full agenda, including habits, ANCHORs and stubs listings"
      ((agenda "" nil)
       (todo "TODO|IN-PROGRESS|WAITING"
             ((org-agenda-overriding-header "Unscheduled TODO items")
              (org-agenda-skip-function
               (quote
                (org-agenda-skip-entry-if
                 (quote scheduled))))))
       (todo "GOAL"
             ((org-agenda-overriding-header "All GOAL headings"))))
      nil)
     (";D" "List all non-archived DONE and ABORTED headings (except habits)"
      ((todo "DONE|ABORTED"
             ((org-agenda-overriding-header "All DONE or ABORTED WORK tasks")
              (org-agenda-tag-filter-preset
               (quote
                ("+WORK")))))
       (todo "DONE|ABORTED"
             ((org-agenda-overriding-header "All DONE or ABORTED non-WORK tasks")
              (org-agenda-tag-filter-preset
               (quote
                ("-WORK-STYLE=\"habit\""))))))
      ((org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote nottodo)
          (quote done)))))))))
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-follow-indirect nil)
 '(org-agenda-inhibit-startup t)
 '(org-agenda-mouse-1-follows-link nil)
 '(org-agenda-prefix-format
   (quote
    ((agenda . " %i %-20:c%-5 e%?-12t% s")
     (timeline . "  % s")
     (todo . " %i %-20:c%-5 e ")
     (tags . " %i %-20:c%-5 e ")
     (search . " %i %-20:c%-5 e "))))
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown
   (quote not-today))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down time-up category-keep priority-down)
     (todo category-keep priority-down)
     (tags category-keep priority-down)
     (search category-keep))))
 '(org-agenda-span
   (quote fortnight))
 '(org-agenda-sticky t)
 '(org-agenda-tags-column 100)
 '(org-agenda-todo-list-sublevels t)
 '(org-agenda-window-frame-fractions
   (quote
    (0.5 . 0.75)))
 '(org-agenda-window-setup
   (quote only-window))
 '(org-bullets-bullet-list
   (quote
    ("●" "◉" "○" "◒" "◓" "✪")))
 '(org-capture-templates
   (quote
    (("A" "Appointment" entry
      (file+olp "~/.orgfiles/general.org" "Appointments")
      "* %^{Title}
  SCHEDULED: %^T" :immediate-finish t :empty-lines 1 :clock-resume t)
     ("s" "Stub." entry
      (file+olp "~/.orgfiles/general.org" "Task Stubs")
      "* TODO %? :stub:%^G
  Created: %U

  %i
  %a" :empty-lines 1 :clock-resume t))))
 '(org-clock-clocktable-default-properties
   (quote
    (:maxlevel 2 :scope file :block thisweek :link t :indent t :narrow 40)))
 '(org-clock-in-switch-to-state "IN-PROGRESS")
 '(org-clock-x11idle-program-name "xprintidle")
 '(org-columns-default-format
   "%45ITEM(Item) %TODO %3PRIORITY(Prio) %Effort(Est.){:} %CLOCKSUM(Total) %CLOCKSUM_T(Today) %12CATEGORY(Cat) %TAGS")
 '(org-crypt-disable-auto-save t)
 '(org-ctags-open-link-functions nil)
 '(org-custom-properties
   (quote
    ("STUB_TYPE_FLAG")))
 '(org-default-priority 66)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends
   (quote
    (ascii html icalendar latex md odt taskjuggler)))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs))))
 '(org-habit-graph-column 70)
 '(org-habit-show-habits t)
 '(org-habit-show-habits-only-for-today t)
 '(org-highest-priority 65)
 '(org-highlight-latex-and-related
   (quote
    (latex entities)))
 '(org-id-link-to-org-use-id
   (quote create-if-interactive))
 '(org-indirect-buffer-display
   (quote other-window))
 '(org-journal-date-format "%A, %Y-%m-%d")
 '(org-journal-dir "~/.orgfiles/journal/")
 '(org-journal-enable-encryption t)
 '(org-journal-file-format "%Y-%m-%d")
 '(org-keep-stored-link-after-insertion t)
 '(org-level-color-stars-only t)
 '(org-link-search-must-match-exact-headline t)
 '(org-log-done
   (quote note))
 '(org-log-into-drawer t)
 '(org-log-note-clock-out nil)
 '(org-log-redeadline
   (quote note))
 '(org-log-refile
   (quote time))
 '(org-log-reschedule
   (quote note))
 '(org-lowest-priority 67)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-crypt org-ctags org-gnus org-habit org-id org-info org-irc org-mhe org-mouse org-rmail org-w3m org-bullets)))
 '(org-pretty-entities t)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-refile-targets
   (quote
    ((org-agenda-files :maxlevel . 2))))
 '(org-tag-faces nil)
 '(org-tag-persistent-alist
   (quote
    ((:startgroup)
     ("WORK" . 87)
     ("PERSONAL" . 80)
     ("CHORE" . 67)
     (:endgroup)
     (:newline)
     ("stub" . 115)
     ("vague" . 118)
     ("nonatomic" . 110)
     ("report_back" . 114)
     ("REF" . 82)
     (:newline))))
 '(org-tags-column -77)
 '(org-taskjuggler-default-global-properties
   "shift standard40 \"Standard 40 week\" {
workinghours mon-fri 08:00-17:00
}")
 '(org-todo-keywords
   (quote
    ((type "TODO(t)" "GOAL(g@)" "WAITING(w@/!)" "IN-PROGRESS(i!/@)" "|" "ABORTED(a@)" "DONE(d!)"))))
 '(org-track-ordered-property-with-tag t))
