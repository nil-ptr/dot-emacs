(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    ((company-math-symbols-latex :with company-latex-commands)
     (company-math-symbols-unicode :with company-latex-commands)
     company-c-headers company-ghci company-cabal company-bbdb company-nxml company-css company-eclim company-semantic company-xcode company-cmake company-capf company-files
     (company-dabbrev-code company-gtags company-etags company-keywords)
     company-oddmuse company-dabbrev))
   nil nil "A couple of things to note here.

1. company-math-symbols-unicode and company-math-symbols-latex will
   both be active in org-mode with this setup. However completions
   will go for the unicode variant by default. The latex version only
   activates when the character after point uses a latex-like font. So
   typing \"\\sigm\" will suggest suggest \"σ\" by default, but will
   suggest \"\\sigma\" if completion is attempted inside something like
   \"\\symbf{}\".

2. company-glsl is installed, but not currently activated, since it
   requires glslValidator to be installed.")
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-ghc-show-info t)
 '(company-idle-delay 0)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip
   ((t
     (:background "dim gray" :foreground "black"))))
 '(company-tooltip-annotation
   ((t
     (:foreground "deep sky blue"))))
 '(company-tooltip-annotation-selection
   ((t
     (:inherit company-tooltip-annotation :foreground "black"))))
 '(company-tooltip-common
   ((t
     (:foreground "gold" :weight bold))))
 '(company-tooltip-common-selection
   ((t
     (:inherit company-tooltip-common :foreground "black")))))
