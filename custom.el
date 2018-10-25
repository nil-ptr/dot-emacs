;;; custom.el
;;; Commentary:
;;; Contains the variables set by customize.
;;;
;;; WARNING: DO NOT EDIT THIS MANUALLY!
;;;
;;; You can, but you shouldn't. See the customize manual for more info.
;;;
;;; Created: 2018-10-17

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-input-idle-jump-delay 1.0)
 '(ace-isearch-input-length 4)
 '(ace-isearch-use-fallback-function nil)
 '(alert-default-style (quote libnotify))
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(centered-window-mode nil)
 '(company-backends
   (quote
    (company-cabal company-bbdb company-nxml company-css company-eclim company-semantic company-xcode company-cmake company-capf company-files
                   (company-dabbrev-code company-gtags company-etags company-keywords)
                   company-oddmuse company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-ghc-show-info t)
 '(company-idle-delay 0)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("7644fe0d5397306ba4858b13a582e453c41ba2bbd3d68d7d8260eb16d320dcea" "fb448aeeac872fac5b33abd384eae8466ee441c79ae4e68fd6c5dd2aff38bc4f" "55adfb43cffb126f577d4d51c2cc30b3c69686d8465fff9b59bb1ef08a21bacf" "29ed69e6bbb7cb253d964bc47ae5b77bb0aa7bfebde1b52ccb15de059631648f" "868f73b5cf78e72ca2402e1d48675e49cc9a9619c5544af7bf216515d22b58e7" default)))
 '(default-frame-alist (quote ((vertical-scroll-bars))))
 '(dhall-format-at-save nil)
 '(encourage-mode t)
 '(ensime-sbt-command "/usr/bin/sbt")
 '(explicit-shell-file-name "/bin/bash")
 '(find-directory-functions (quote (sr-dired cvs-dired-noselect dired-noselect)))
 '(flycheck-check-syntax-automatically (quote (save new-line mode-enabled)))
 '(flycheck-clang-args (quote ("-fPIE")))
 '(flycheck-clang-include-path nil)
 '(flycheck-clang-language-standard "c++11")
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(flycheck-gcc-include-path nil)
 '(flycheck-ghc-args (quote ("-v")))
 '(flycheck-ghc-language-extensions nil)
 '(flycheck-ghc-no-user-package-database nil)
 '(flycheck-ghc-package-databases nil)
 '(flycheck-ghc-search-path nil)
 '(flycheck-global-modes (quote (not org-agenda-mode org-mode)))
 '(flycheck-haskell-ghc-executable "/opt/ghc/bin/ghc")
 '(flycheck-haskell-runghc-command (quote ("/opt/ghc/bin/runghc" "-i")))
 '(flycheck-hlint-ignore-rules nil)
 '(flycheck-hlintrc nil)
 '(flycheck-pos-tip-timeout 5)
 '(flyspell-default-dictionary "english")
 '(fortune-cookie-mode nil)
 '(global-ace-isearch-mode t)
 '(haskell-hoogle-command "stack hoogle --")
 '(haskell-mode-hook
   (quote
    (capitalized-words-mode flyspell-prog-mode interactive-haskell-mode hlint-refactor-mode)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "/opt/ghc/bin/ghci")
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(helm-autoresize-mode t)
 '(helm-dash-browser-func (quote eww))
 '(hi2-show-indentations-after-eol nil)
 '(hi2-show-indentations-delay 0.5)
 '(idris-enable-elab-prover t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message "What little self-esteem I have left is coffee.
")
 '(linum-delay t)
 '(magit-log-arguments (quote ("-n256" "--graph" "--decorate")))
 '(markdown-fontify-code-blocks-natively t)
 '(nyan-animate-nyancat nil)
 '(nyan-bar-length 24)
 '(nyan-wavy-trail nil)
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
     ("u" "Unscheduled or \"overdue\" TODO items (excluding habits)" tags-todo "-SCHEDULED>=\"<now>\"-STYLE=\"habit\"-TODO={DONE\\|ABORTED}"
      ((org-deadline-warning-days 30)))
     (";" . "Multi block agenda views")
     (";W" "Agenda, coupled with WORK tasks and WORK stubs"
      ((agenda "" nil)
       (tags-todo "+WORK-SCHEDULED>=\"<now>\"-STYLE=\"habit\""
                  ((org-agenda-overriding-header "Unscheduled or \"overdue\" TODO headings tagged WORK")
                   (org-deadline-warning-days 30)
                   (org-agenda-skip-function
                    (quote
                     (org-agenda-skip-entry-if
                      (quote todo)
                      (quote
                       ("GOAL" "DONE" "ABORTED")))))))
       (tags-todo "WORK+SCHEDULED>=\"<now>\"-STYLE=\"habit\""
                  ((org-agenda-overriding-header "TODO headings tagged as WORK, scheduled in the future")
                   (org-agenda-skip-function
                    (quote
                     (org-agenda-skip-entry-if
                      (quote todo)
                      (quote
                       ("GOAL" "DONE" "ABORTED")))))))
       (tags-todo "WORK"
                  ((org-agenda-overriding-header "All GOALs tagged as WORK")
                   (org-agenda-skip-function
                    (quote
                     (org-agenda-skip-entry-if
                      (quote nottodo)
                      (quote
                       ("GOAL")))))))
       (tags "WORK+stub-STUB_TYPE_FLAG={PHONY\\|IGNORE}"
             ((org-agenda-overriding-header "All headings tagged as WORK and stub"))))
      ((org-agenda-tag-filter-preset
        (quote
         ("-CHORE")))
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote todo)
          (quote done))))
       (org-agenda-files
        (quote
         ("~/edu/proj_niclas_2018-07-05/proj.org" "~/edu/5dv180/course.org" "~/edu/5EL006/course.org" "~/.orgfiles/akeexj.org" "~/.orgfiles/general.org")))))
     (";;" "Full agenda, including habits, ANCHORs and stubs listings"
      ((agenda "" nil)
       (tags "INBOX+stub-STUB_TYPE_FLAG={PHONY\\|IGNORE}"
             ((org-agenda-overriding-header "Stub entries in INBOX trees")))
       (tags-todo "-SCHEDULED>=\"<now>\"-STYLE=\"habit\""
                  ((org-agenda-overriding-header "Unscheduled and \"overdue\" TODO headings")
                   (org-agenda-skip-function
                    (quote
                     (org-agenda-skip-entry-if
                      (quote todo)
                      (quote
                       ("GOAL" "DONE" "ABORTED")))))))
       (todo "GOAL"
             ((org-agenda-overriding-header "All GOAL headings")))
       (tags "ANCHOR-SCHEDULED>=\"<now>\""
             ((org-agenda-overriding-header "Unscheduled and \"overdue\" ANCHOR headings"))))
      ((org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote todo)
          (quote done))))))
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
 '(org-agenda-files
   (quote
    ("~/edu/proj_niclas_2018-07-05/proj.org" "~/code/haskell/personal/emoric/emoric.org" "~/edu/5dv180/course.org" "~/edu/5EL006/course.org" "~/.orgfiles/gbf_stuff.org" "~/.orgfiles/akeexj.org" "~/.orgfiles/gsons.org" "~/.orgfiles/local.org" "~/.orgfiles/general.org")))
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
 '(org-agenda-skip-scheduled-if-deadline-is-shown (quote not-today))
 '(org-agenda-skip-scheduled-if-done nil)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down time-up category-keep priority-down)
     (todo category-keep priority-down)
     (tags category-keep priority-down)
     (search category-keep))))
 '(org-agenda-span (quote fortnight))
 '(org-agenda-sticky t)
 '(org-agenda-tags-column 100)
 '(org-agenda-todo-list-sublevels t)
 '(org-agenda-window-frame-fractions (quote (0.5 . 0.75)))
 '(org-agenda-window-setup (quote only-window))
 '(org-bullets-bullet-list (quote ("●" "◉" "○" "◒" "◓" "✪")))
 '(org-clock-clocktable-default-properties
   (quote
    (:maxlevel 2 :scope file :block thisweek :link t :indent t :narrow 40)))
 '(org-clock-in-switch-to-state "IN-PROGRESS")
 '(org-clock-x11idle-program-name "xprintidle")
 '(org-columns-default-format
   "%45ITEM(Item) %TODO %3PRIORITY(Prio) %Effort(Est.){:} %CLOCKSUM(Total) %CLOCKSUM_T(Today) %12CATEGORY(Cat) %TAGS")
 '(org-crypt-disable-auto-save t)
 '(org-ctags-open-link-functions nil)
 '(org-default-priority 66)
 '(org-enforce-todo-dependencies t)
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
 '(org-highlight-latex-and-related (quote (latex script entities)))
 '(org-id-link-to-org-use-id t)
 '(org-indirect-buffer-display (quote other-window))
 '(org-journal-date-format "%A, %Y-%m-%d")
 '(org-journal-dir "~/.orgfiles/journal/")
 '(org-journal-enable-encryption t)
 '(org-journal-file-format "%Y-%m-%d")
 '(org-keep-stored-link-after-insertion t)
 '(org-link-search-must-match-exact-headline t)
 '(org-log-done (quote note))
 '(org-log-into-drawer t)
 '(org-log-note-clock-out nil)
 '(org-log-redeadline (quote note))
 '(org-log-refile (quote time))
 '(org-log-reschedule (quote note))
 '(org-lowest-priority 67)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-crypt org-ctags org-gnus org-habit org-id org-info org-irc org-mhe org-mouse org-rmail org-w3m org-bullets)))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 2))))
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
 '(org-todo-keywords
   (quote
    ((type "TODO(t)" "GOAL(g@)" "WAITING(w@/!)" "IN-PROGRESS(i!/@)" "|" "ABORTED(a@)" "DONE(d!)"))))
 '(org-track-ordered-property-with-tag t)
 '(package-check-signature (quote allow-unsigned))
 '(package-selected-packages
   (quote
    (ess flycheck org-wild-notifier lsp-haskell lsp-java lsp-mode lsp-rust company-glsl dhall-mode hlint-refactor org-bullets pdf-tools org-journal wgrep-helm zen-and-art-theme yatemplate yaml-mode whitespace-cleanup-mode web-mode waher-theme vagrant use-package travis totd toml-mode toml sunrise-x-tree sunrise-x-modeline sunrise-x-mirror sunrise-x-loop smex scion rust-playground rainbow-delimiters racer prodigy powerline popwin php-mode pandoc-mode pallet ox-pandoc org-pomodoro nyan-mode multiple-cursors markdown-mode magithub magit-gh-pulls magit-find-file magit-filenotify llvm-mode liquid-types kotlin-mode kanji-mode jtags java-snippets java-imports idle-highlight-mode ibuffer-projectile htmlize highlight-indentation helm-unicode helm-themes helm-projectile helm-org-rifle helm-mode-manager helm-make helm-idris helm-gtags helm-gitignore helm-git helm-flyspell helm-flycheck helm-descbinds helm-dash helm-company helm-c-yasnippet helm-ag helm-R haskell-snippets haskell-emacs-text haskell-emacs-base groovy-mode gradle-mode glsl-mode git-ps1-mode ggtags flycheck-tip flycheck-stack flycheck-pos-tip flycheck-ocaml flycheck-kotlin flycheck-hdevtools flycheck-haskell flycheck-elm flycheck-color-mode-line flycheck-cask firecode-theme expand-region exec-path-from-shell ensime encourage-mode drag-stuff d-mode company-quickhelp company-math company-ghci company-cabal company-c-headers column-enforce-mode circe cask-mode buffer-move avy-zap auto-yasnippet ample-theme 2048-game)))
 '(powerline-height nil)
 '(powerline-text-scale-factor nil)
 '(projectile-ignored-projects (quote ("~" "~/.emacs.d")))
 '(projectile-keymap-prefix nil)
 '(rainbow-delimiters-max-face-count 8)
 '(rainbow-identifiers-cie-l*a*b*-lightness 25)
 '(rainbow-identifiers-cie-l*a*b*-saturation 40)
 '(safe-local-variable-values
   (quote
    ((column-enforce-column . 70)
     (column-enforce-column . 65)
     (column-enforce-column . 80))))
 '(scroll-bar-mode nil)
 '(show-trailing-whitespace nil)
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 30)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
 '(speedbar-frame-plist
   (quote
    (minibuffer nil width 30 border-width 0 internal-border-width 0 unsplittable t default-toolbar-visible-p nil has-modeline-p nil menubar-visible-p nil default-gutter-visible-p nil)))
 '(speedbar-show-unknown-files t)
 '(speedbar-supported-extension-expressions
   (quote
    (".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".ad[abs]" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?" ".toml" ".rs" ".hs" ".cabal")))
 '(speedbar-use-images nil)
 '(split-height-threshold 90)
 '(split-width-threshold 90)
 '(sr-speedbar-default-width 30)
 '(sr-speedbar-delete-windows t)
 '(sublimity-mode t)
 '(sublimity-scroll-weight 4.0)
 '(tab-width 4)
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(use-file-dialog nil)
 '(whitespace-action nil)
 '(writeroom-border-width 80)
 '(writeroom-fringes-outside-margins nil)
 '(writeroom-global-effects
   (quote
    (writeroom-toggle-fullscreen writeroom-toggle-internal-border-width)))
 '(writeroom-mode-line t)
 '(yas-also-auto-indent-first-line t)
 '(yas-indent-line (quote auto))
 '(yas-prompt-functions
   (quote
    (yas-dropdown-prompt yas-completing-prompt yas-ido-prompt yas-no-prompt)))
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets" yas-installed-snippets-dir)))
 '(yas-triggers-in-field t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(ace-jump-face-foreground ((t (:background "wheat" :foreground "dark violet"))))
 '(agda2-highlight-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(flycheck-error ((t (:underline (:color "red" :style wave)))))
 '(flycheck-info ((t (:underline (:color "lime green" :style wave)))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "sea green"))))
 '(fringe ((t (:background "#242424"))))
 '(ghc-face-error ((t (:underline "orange red"))))
 '(ghc-face-hole ((t (:underline "purple"))))
 '(ghc-face-warn ((t (:underline "gold"))))
 '(haskell-constructor-face ((t (:foreground "navajo white" :weight bold))))
 '(highlight-indentation-current-column-face ((t (:background "gray10"))))
 '(highlight-indentation-face ((t (:inherit fringe :background "gray18"))))
 '(idris-semantic-bound-face ((t (:foreground "gray50" :weight extra-bold))))
 '(idris-semantic-data-face ((t (:foreground "NavajoWhite3" :weight semi-bold))))
 '(idris-semantic-function-face ((t (:inherit font-lock-function-name-face))))
 '(idris-semantic-type-face ((t (:inherit font-lock-type-face :foreground "dark turquoise"))))
 '(linum ((t (:inherit (shadow default)))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "#2b2b2b"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "gray55"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "orange red"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "light slate blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "DarkOrange2"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "dark orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "goldenrod2"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "violet red"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "LightGoldenrod1"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "white smoke")))))
