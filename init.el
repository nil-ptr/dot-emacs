;;; init ---  My init file. -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Self-recompiling init file that loads the contents of my literate
;;; init files, found in the init subfolder.
;;;
;;; See CHANGELOG.md for changes.

;;; Code:

;; Init timing
(message "Starting timer for the loading of %s..." load-file-name)
  (defconst emacs-start-time (current-time))

;; Useful to find out what caused a package to load early for
;; seemingly no reason.
;(eval-after-load "yasnippet" '(debug))

;;; -------------------------------------------------- ;;;
;;; PACKAGE.EL SETUP                                   ;;;
;;; -------------------------------------------------- ;;;


;; Idea borrowed from a code example on the getting started page of
;; melpa. I extended the example by only running this part when
;; compiling/recompiling init.el.
(setq package-archives
 (eval-when-compile
   (require 'package)
   (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                       (not (gnutls-available-p))))
          (proto (if no-ssl "http" "https")))
     (when no-ssl
       (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
     ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
     (add-to-list 'package-archives
                  (cons "melpa" (concat proto "://melpa.org/packages/")) t)
     ;;(add-to-list 'package-archives
     ;;  (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
     (when (< emacs-major-version 24)
       ;; For important compatibility libraries like cl-lib
       (add-to-list
        'package-archives
        (cons "gnu"
              (concat proto "://elpa.gnu.org/packages/"))))
     (add-to-list 'package-archives
                  (cons "org"
                        (concat proto "://orgmode.org/elpa/"))))
   ;; Return the arrived at list.
   package-archives))

;; Initialize packages, adjust load-path.
(eval-and-compile
  (package-initialize)
  (defconst my-init-elisp-dir (expand-file-name "init/elisp"
                                                user-emacs-directory))
  (add-to-list 'load-path my-init-elisp-dir))

;; When compiling: Load the needed macro declarations, and make sure
;; use-package is available. The rest of the init process depends on
;; it.
;;
;; In fact, if it's missing, try to install it right here and now
;; before proceeding.
(eval-when-compile
  (require 'my-load-macros)
  (require 'my-init-macros)
  ;; check if *any* use-package is around
  (unless (require 'use-package nil 'noerror)
    (init-say "use-package not found..")
    (init-say "querying user for permission to install..")
    (if (y-or-n-p-with-timeout
         "Package use-package not found.  Download use-package?"
         10
         nil)
        (progn
          (init-say "permission granted; installing use-package..")
          (package-refresh-contents)
          (package-install 'use-package))
      (init-say "permission not granted; init success doubtful..")
      (error "Cannot proceed without use-package!"))))


;;; -------------------------------------------------- ;;;
;;; SELF-RECOMPILATION MACHINERY                       ;;;
;;; -------------------------------------------------- ;;;

(eval-when-compile
  (defsubst my--bound-and-eq (v value)
    "Return t if V is bound and `eq' to VALUE."
    (and (boundp v) (eq v value))))

;; Actual recompilation machinery.
(init-say "Running self-recompile check..")
(defvar my--init-boot-important-flag-do-not-touch nil)
(let ((ok
       (my-load-may-compile-el
        (emacs-path "init")
        t
        (elispfiles-path "my-init-macros.el")
        (elispfiles-path "my-load-macros.el")
        (emacs-path "elpa") ;; package.el installs packages here
        )))
  (cond

   ;; Recompiling, set flag to prevent loading begin.el twice.
   ((and (numberp ok) (= 1 ok))
    (progn
      (init-say
       "(old): init.elc was outdated or deps changed; recompiled")
      (setq my--init-boot-important-flag-do-not-touch 'init)))

   ;; Everything is ok, carry on.
   ((and (numberp ok) (= 0 ok))
    (setq my--init-boot-important-flag-do-not-touch t))

   ;; Failure, set flag to prevent loading anything further.
   (t
    (progn
      (error
       "Error in init.el: self-compile failed: %s" ok)
      (setq my--init-boot-important-flag-do-not-touch 'initfail)))))

;;; -------------------------------------------------- ;;;
;;; GUARD AGAINST EVALUATING TWICE                     ;;;
;;; -------------------------------------------------- ;;;


;;; Actual init logic:
(when (my--bound-and-eq my--init-boot-important-flag-do-not-touch t)
  (init-say "Proceeding with initialisation..")


;;; -------------------------------------------------- ;;;
;;; DIRTY FIXES                                        ;;;
;;; -------------------------------------------------- ;;;


;; This is intended to stop gconf from messing with my fonts.
(define-key special-event-map [config-changed-event] 'ignore)


;;; -------------------------------------------------- ;;;
;;; BASIC SETUP AND HOUSEKEEPING                       ;;;
;;; -------------------------------------------------- ;;;

;; Ensure that these are available before proceeding.
(use-package bind-key
  :ensure use-package)
(use-package diminish
  :ensure t)

;; Need an auto-load from here.
(use-package use-package-bind-key
  :ensure use-package)

;; Fixes a potential keyboard issue.
(require 'iso-transl)

;; set custom file name
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;;; -------------------------------------------------- ;;;
;;; LITERATE INIT IMPORTS                              ;;;
;;; -------------------------------------------------- ;;;



;; My literate emacs init files are in this dir
(eval-and-compile
  (defconst lit-emacs-init-dir (expand-file-name "init" user-emacs-directory)))


;; Some machinery. I may chose to move some of this out of here later.
(eval-when-compile
  (defsubst my-load-init-exp-names (names)
    (mapcar (lambda (x)
               (expand-file-name
                (concat "lit-emacs-init-" x)
                lit-emacs-init-dir))
            names))

  (defsubst my-load-init-imports (&rest names)
    (let* ((exp-names  (my-load-init-exp-names names))
           (pkg-dir-mod-time
            (nth 5 (my--init-file-attributes package-user-dir)))
           (recomplist
            (my-load-check-org-elc-freshness exp-names pkg-dir-mod-time)))
      (if (null recomplist)
          (mapc (lambda (f) (load-file (concat f ".elc"))) exp-names)

        ;; Signal that this is about to happen.
        (init-say "Init files in need of (re-)building: %s" recomplist)

        ;; Tangle imports, if needed
        (require 'ob-tangle) ; in scope, if package-initialize did its job
        (mapc (lambda(f)
                (init-say "Generated %s"
                          (car (last (org-babel-tangle-file
                                      (concat f ".org")
                                      (concat f ".el")
                                      "emacs-lisp")))))
              recomplist)

        ;; Compile those that need it, just load those that do
        ;; not.
        (mapc (lambda (f)
                (if (member f recomplist)
                    (init-say "Compiled and loaded %s"
                              (progn
                                (byte-compile-file (concat f ".el") t)
                                (concat f ".el")))
                  (load-file (concat f ".elc"))))
              exp-names)))))

;;; -------------------------------------------------- ;;;
;;; LOAD LITERATE INIT IMPORTS                         ;;;
;;; -------------------------------------------------- ;;;



(my-load-init-imports

  "general"     ;; The "misc" import, basically.
  "helm"        ;; Sets up the helm-* packages I use.
  "org"         ;; Some setup for org-mode.
  "linum"       ;; Some setup to make global-linum-mode behave.
  "templating"  ;; yas, aya, yatemplate setup.

  )

;;; -------------------------------------------------- ;;;
;;; LOAD CUSTOM FILE                                   ;;;
;;; -------------------------------------------------- ;;;


;; load custom vars
(load custom-file)

;;; -------------------------------------------------- ;;;
;;; THE END                                            ;;;
;;; -------------------------------------------------- ;;;




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

;;; -------------------------------------------------- ;;;
;;; VERY IMPORTANT PAREN DO NOT DELETE                 ;;;
;;; -------------------------------------------------- ;;;


) ; This is an important paren. There are many like it, but this one
  ; is necessary to close the '(when' starting under 'Actual init logic:'.


;;; End of Actual init logic

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
