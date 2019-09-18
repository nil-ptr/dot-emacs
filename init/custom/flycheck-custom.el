(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-check-syntax-automatically
   (quote
    (save new-line mode-enabled)))
 '(flycheck-clang-args
   (quote
    ("-fPIE")))
 '(flycheck-clang-include-path nil)
 '(flycheck-clang-language-standard "c++11")
 '(flycheck-display-errors-function
   (quote flycheck-pos-tip-error-messages))
 '(flycheck-gcc-include-path nil)
 '(flycheck-ghc-args
   (quote
    ("-v")))
 '(flycheck-ghc-language-extensions nil)
 '(flycheck-ghc-no-user-package-database nil)
 '(flycheck-ghc-package-databases nil)
 '(flycheck-ghc-search-path nil)
 '(flycheck-global-modes
   (quote
    (not org-agenda-mode org-mode)))
 '(flycheck-haskell-ghc-executable "/opt/ghc/bin/ghc")
 '(flycheck-haskell-runghc-command
   (quote
    ("/opt/ghc/bin/runghc" "-i")))
 '(flycheck-hlint-ignore-rules nil)
 '(flycheck-hlintrc nil)
 '(flycheck-pos-tip-timeout 5))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error
   ((t
     (:underline
      (:color "red" :style wave)))))
 '(flycheck-info
   ((t
     (:underline
      (:color "lime green" :style wave))))))
