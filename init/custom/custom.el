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
 '(alert-default-style
   (quote libnotify))
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auto-insert-mode t)
 '(centered-window-mode nil)
 '(custom-enabled-themes
   (quote
    (wombat)))
 '(custom-safe-themes
   (quote
    ("7644fe0d5397306ba4858b13a582e453c41ba2bbd3d68d7d8260eb16d320dcea" "fb448aeeac872fac5b33abd384eae8466ee441c79ae4e68fd6c5dd2aff38bc4f" "55adfb43cffb126f577d4d51c2cc30b3c69686d8465fff9b59bb1ef08a21bacf" "29ed69e6bbb7cb253d964bc47ae5b77bb0aa7bfebde1b52ccb15de059631648f" "868f73b5cf78e72ca2402e1d48675e49cc9a9619c5544af7bf216515d22b58e7" default)))
 '(default-frame-alist
    (quote
     ((vertical-scroll-bars))))
 '(dhall-format-at-save nil)
 '(encourage-mode t)
 '(ensime-sbt-command "/usr/bin/sbt")
 '(exec-path
   (quote
    ("/opt/ghc/bin" "/home/nils/bin" "/home/nils/.cabal/bin" "/home/nils/.local/bin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/local/games" "/usr/games" "/usr/local/libexec/emacs/26.2/x86_64-pc-linux-gnu")))
 '(explicit-shell-file-name "/bin/bash")
 '(find-directory-functions
   (quote
    (cvs-dired-noselect dired-noselect)))
 '(flyspell-default-dictionary "english")
 '(fortune-cookie-mode nil)
 '(global-ace-isearch-mode t)
 '(global-linum-disabled-modes-list
   (quote
    (eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode doc-view-mode pdf-view-mode haskell-interactive-mode magit-mode magit-status-mode magit-log-mode magit-refs-mode magit-cherry-mode magit-process-mode)))
 '(haskell-hoogle-command "stack hoogle --")
 '(haskell-mode-hook
   (quote
    (capitalized-words-mode flyspell-prog-mode interactive-haskell-mode hlint-refactor-mode)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "/opt/ghc/bin/ghci")
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type
   (quote cabal-repl))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(helm-autoresize-mode t)
 '(hi2-show-indentations-after-eol nil)
 '(hi2-show-indentations-delay 0.5)
 '(idris-enable-elab-prover t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(initial-major-mode
   (quote org-mode))
 '(initial-scratch-message
   "#+TITLE: Scratch

* Notes

  What little self-esteem I have left is coffee..

")
 '(initsplit-customizations-alist
   (quote
    (("^org-agenda-custom-commands$" "local-custom.el" t t)
     ("^org-agenda-files$" "local-custom.el" t t)
     ("^company" "company-custom.el" t t)
     ("^org" "org-custom.el" t t)
     ("^flycheck" "flycheck-custom.el" t t)))
   nil nil "Important!

Customization options that should be kept strictly machine local (like
org-agenda-files), ought to be matched *exactly* and added to
local-custom.el. Make sure these patterns occur before any more general
patterns, so that these customizations aren't accidentally written
elsewhere.

Other splits should generally follow the convention used above:
filename sans \"-custom.el\" and pattern prefix should match.

IMPORTANT: Be sure to not use filenames that might conflict with
library names. Doing so may have strange results, e.g. calling the
\"org-custom.el\" file \"org.el\" will cause org related after-load-alist
items to fire, even though they're entirely uncalled for.")
 '(initsplit-default-directory "~/.emacs.d/init/custom")
 '(initsplit-pretty-print t)
 '(linum-delay t)
 '(magit-log-arguments
   (quote
    ("-n256" "--graph" "--decorate")))
 '(markdown-enable-math t)
 '(markdown-fontify-code-blocks-natively t)
 '(package-check-signature
   (quote allow-unsigned))
 '(package-selected-packages
   (quote
    (multiple-cursors company-stan eldoc-stan flycheck-stan org-picklink hlint-refactor delight auto-yasnippet avy avy-zap buffer-move column-enforce-mode company company-c-headers company-cabal company-ghci company-glsl company-math company-quickhelp diminish encourage-mode flycheck flycheck-haskell flycheck-pos-tip flyspell haskell-mode helm helm-ag helm-c-yasnippet helm-company helm-flycheck helm-flyspell helm-org-rifle helm-projectile highlight-indentation htmlize magit markdown-mode matlab-mode org-bullets org-plus-contrib pandoc-mode pdf-tools pos-tip powerline projectile rainbow-delimiters smartparens use-package whitespace-cleanup-mode yasnippet yatemplate)))
 '(pcomplete-command-completion-function
   (lambda nil
     (progn
       (message "pcomplete-command-completion-function called!")
       nil))
   nil nil "I'm not sure when this gets called, so I'm adding a message here. Aside from that, it's disabled for the same reason that the pcomplete-default-completion-function is.")
 '(pcomplete-default-completion-function
   (lambda nil nil)
   nil nil "I don't really like the default behaviour, so I'm shutting it off. Specific modes can still define pcompletion though.")
 '(powerline-display-mule-info t)
 '(powerline-gui-use-vcs-glyph nil)
 '(powerline-height nil)
 '(powerline-narrowed-indicator "⭿")
 '(powerline-text-scale-factor nil)
 '(projectile-ignored-projects
   (quote
    ("~" "~/.emacs.d")))
 '(projectile-keymap-prefix nil)
 '(projectile-track-known-projects-automatically nil)
 '(rainbow-delimiters-max-face-count 8)
 '(rainbow-identifiers-cie-l*a*b*-lightness 25)
 '(rainbow-identifiers-cie-l*a*b*-saturation 40)
 '(safe-local-variable-values
   (quote
    ((flycheck-disabled-checkers haskell-stack-ghc)
     (flycheck-ghc-package-databases "/home/nils/.cabal/store/ghc-8.8.1/package.db" "dist-newstyle/packagedb/ghc-8.8.1")
     (column-enforce-column . 70)
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
 '(split-height-threshold 70)
 '(split-width-threshold 120)
 '(tab-width 4)
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines t)
 '(use-file-dialog nil)
 '(whitespace-action nil)
 '(yas-also-auto-indent-first-line t)
 '(yas-indent-line
   (quote auto))
 '(yas-prompt-functions
   (quote
    (yas-dropdown-prompt yas-completing-prompt yas-ido-prompt yas-no-prompt)))
 '(yas-snippet-dirs
   (quote
    ("~/.emacs.d/snippets"))
   nil nil "Removed the \"yas-installed-snippets-dir\" variable from this list, since that variable was made obsolete some time ago.")
 '(yas-triggers-in-field t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default
    ((t
      (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(ace-jump-face-foreground
   ((t
     (:background "wheat" :foreground "dark violet"))))
 '(agda2-highlight-keyword-face
   ((t
     (:inherit font-lock-keyword-face))))
 '(font-lock-doc-face
   ((t
     (:inherit font-lock-string-face :foreground "sea green"))))
 '(fringe
   ((t
     (:background "#242424"))))
 '(ghc-face-error
   ((t
     (:underline "orange red"))))
 '(ghc-face-hole
   ((t
     (:underline "purple"))))
 '(ghc-face-warn
   ((t
     (:underline "gold"))))
 '(haskell-constructor-face
   ((t
     (:foreground "navajo white" :weight bold))))
 '(helm-selection
   ((t
     (:distant-foreground "black" :box
                          (:line-width 3 :color "forest green" :style released-button)))))
 '(highlight-indentation-current-column-face
   ((t
     (:background "gray10"))))
 '(highlight-indentation-face
   ((t
     (:inherit fringe :background "gray18"))))
 '(idris-semantic-bound-face
   ((t
     (:foreground "gray50" :weight extra-bold))))
 '(idris-semantic-data-face
   ((t
     (:foreground "NavajoWhite3" :weight semi-bold))))
 '(idris-semantic-function-face
   ((t
     (:inherit font-lock-function-name-face))))
 '(idris-semantic-type-face
   ((t
     (:inherit font-lock-type-face :foreground "dark turquoise"))))
 '(linum
   ((t
     (:inherit
      (shadow default)))))
 '(markdown-code-face
   ((t
     (:inherit fixed-pitch :background "#2b2b2b"))))
 '(rainbow-delimiters-depth-1-face
   ((t
     (:foreground "gray55"))))
 '(rainbow-delimiters-depth-2-face
   ((t
     (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-3-face
   ((t
     (:foreground "orange red"))))
 '(rainbow-delimiters-depth-4-face
   ((t
     (:foreground "light slate blue"))))
 '(rainbow-delimiters-depth-5-face
   ((t
     (:foreground "DarkOrange2"))))
 '(rainbow-delimiters-depth-6-face
   ((t
     (:foreground "dark orchid"))))
 '(rainbow-delimiters-depth-7-face
   ((t
     (:foreground "goldenrod2"))))
 '(rainbow-delimiters-depth-8-face
   ((t
     (:foreground "violet red"))))
 '(rainbow-delimiters-depth-9-face
   ((t
     (:foreground "LightGoldenrod1"))))
 '(rainbow-delimiters-unmatched-face
   ((t
     (:foreground "white smoke")))))
