;;; begin --- My actual init file. Duh. -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Nothing much to say here.
;;;
;;; See CHANGELOG.md for changes.

;;; Code:

;; Init timing
(message "Starting timer for the loading of %s..." load-file-name)
  (defconst emacs-start-time (current-time))

;; Useful to find out what caused a package to load early for
;; seemingly no reason.
;(eval-after-load "yasnippet" '(debug))

;;; ------------------------------ ;;;
;;; DIRTY FIXES                    ;;;
;;; ------------------------------ ;;;



;; This is intended to stop gconf from messing with my fonts.
(define-key special-event-map [config-changed-event] 'ignore)
;;; end of dirty fixes


;;; ------------------------------ ;;;
;;; SOME SETUP                     ;;;
;;; ------------------------------ ;;;

(eval-and-compile
  (require 'cask "~/.cask/cask.el")
  (defconst this-init-bundle-thing (cask-initialize)))
;(eval-when-compile (defconst this-init-bundle-thing nil))

;; Activate cask
;; Or don't. Whatever.
;; (eval-when-compile
;;   (require 'cask "~/.cask/cask.el")
;;   ;(add-to-list 'load-path (expand-file-name ".cask" user-emacs-directory))
;;   ;; Capture the load-path for future use
;;   (let* ((my-bundle (or this-init-bundle-thing
;;                         (cask-setup (expand-file-name
;;                                      user-emacs-directory))))
;;          (my-lp (cask-load-path my-bundle))
;;          (my-deps (cask-runtime-dependencies my-bundle t))
;;          (my-extended-deps (cask-dependencies my-bundle t))
;;          (my-dep-ex-names
;;           (mapcar #'(lambda (x)
;;                       (symbol-name (cask-dependency-name x))) my-extended-deps))
;;          (my-dep-names
;;           (mapcar #'(lambda (x)
;;                       (symbol-name (cask-dependency-name x))) my-deps)))
;;     (defmacro init-return-cask-load-path()
;;       `(list ,@(seq-filter
;;                 (lambda (x)
;;                   ;; Skip my literal .emacs.d directory
;;                   (not (string= x (expand-file-name user-emacs-directory))))
;;                 my-lp)))
;;     (defmacro init-print-deps()
;;       `(message "\nruntime: %s\n\ncompile:%s" (list ,@my-dep-names) (list,@my-dep-ex-names)))
;;     (defmacro init-use-pkg-decl(symbol)
;;       `(use-package ,symbol
;;          :defer t))
;;     (defmacro init-make-use-pkg-decls()
;;       `(progn
;;          ,@(mapcar
;;             (lambda(x)
;;               `(init-use-pkg-decl ,(cask-dependency-name x)))
;;             my-deps))
;;       )
;;     ))


;; (init-print-deps)
;; ;; Eval __and__ load here. We need the full load-path for both.
;; (eval-and-compile
;;   (mapc (lambda (x)  (add-to-list 'load-path x))
;;         (init-return-cask-load-path)))


;; Load use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Need an auto-load from here.
(require 'use-package-bind-key)

;(init-make-use-pkg-decls)

;; Needed to satisfy the byte-compiler
;(use-package eieio-compat
;  :commands (eieio--generic-static-symbol-specializers))

;; include pallet
(require 'pallet)
(pallet-mode t)

;; Fixes a potential keyboard issue.
(require 'iso-transl)

;; set custom file name
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;; Load org
(use-package org
  :defer t
  :defines (org-agenda-mode-map)
  :commands (org-store-link
             org-agenda-mode-map
             org-babel-tangle-file
             org-agenda
             org-agenda-redo
             org-agenda-maybe-redo
             org-agenda-redo-all)
  :bind (("H-o C-s" . org-store-link)
         ("H-a"     . org-agenda)
         ))

;;; ------------------------------ ;;;
;;; LITERATE INIT IMPORTS          ;;;
;;; ------------------------------ ;;;

;; My literate emacs init files are in this dir
(eval-when-compile
  (defconst lit-emacs-init-dir
    (expand-file-name "init" user-emacs-directory)))


(use-package my-load-macros
  :load-path "init/elisp")

(defmacro my-load-init-imports(&rest names)
  `(my-import-org
    ,@(mapcar (lambda (x)
                (expand-file-name
                 (concat "lit-emacs-init-" x)
                 lit-emacs-init-dir))
              names)))

(my-load-init-imports

  "general"     ;; The "misc" import, basically.
  "helm"        ;; Sets up the helm-* packages I use.
  "org"         ;; Some setup for org-mode.
  "linum"       ;; Some setup to make global-linum-mode behave.
  "templating"  ;; yas, aya, yatemplate setup.

  )


;;; ------------------------------ ;;;
;;; LOAD CUSTOM FILE               ;;;
;;; ------------------------------ ;;;


;; load custom vars
(load custom-file)

;;; ------------------------------ ;;;
;;; THE END                        ;;;
;;; ------------------------------ ;;;



;; Init load timing
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)


;;;; End of my Code


;;; the end of all things
(provide 'begin)
;;; init.el ends here
