;;; init ---  My init file. -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Self-recompiling init file that loads the contents of my literate
;;; init files, found in the init subfolder.
;;;
;;; See CHANGELOG.md for changes.

;;; Code:

;; Init timing
(unless (boundp 'my--init-boot-important-flag-do-not-touch)
  (message "Starting timer for the loading of %s..." load-file-name)
  (defconst emacs-start-time (current-time)))

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
  ;; These two constants need to be defined before calling certain
  ;; functions in my-init-macros.el.
  ;;
  ;; * my-init-elisp-dir: contains my-init-macros.el
  ;;
  ;; * my-init-custom-dir: contains my split custom files.
  (defconst my-init-elisp-dir (expand-file-name "init/elisp"
                                                user-emacs-directory))
  (defconst my-init-custom-dir (expand-file-name "init/custom"
                                                 user-emacs-directory))
  ;; my-init-macros.el needs to be on the load path.
  (add-to-list 'load-path my-init-elisp-dir))

;; When compiling: Load the needed macro declarations, and make sure
;; use-package is available. The rest of the init process depends on
;; it.
;;
;; In fact, if it's missing, try to install it right here and now
;; before proceeding.
(eval-when-compile
  (require 'my-init-macros)
  (def-init-say "init")
  ;; check if *any* use-package is around
  (unless (require 'use-package nil 'noerror)
    (init-say "use-package not found.." "use-package-check")
    (init-say "querying user for permission to install.." "use-package-check")
    (if (y-or-n-p-with-timeout
         "Package use-package not found.  Download use-package?"
         10
         nil)
        (progn
          (init-say "permission granted; installing use-package..")
          (package-refresh-contents)
          (package-install 'use-package))
      (init-say "permission not granted; init success doubtful.."
                "use-package-check")
      (error "Cannot proceed without use-package!"))))


;;; -------------------------------------------------- ;;;
;;; SELF-RECOMPILATION MACHINERY                       ;;;
;;; -------------------------------------------------- ;;;

(eval-when-compile
  (defsubst my--bound-and-equal (v value)
    "Return t if V is bound and its `symbol-value' is `equal' to VALUE."
    (and (boundp v) (equal (symbol-value v) value)))
  (defsubst my--init-boot-important-flag-is (value)
    "Return t if the init flag is `equal' to VALUE."
    (my--bound-and-equal
     'my--init-boot-important-flag-do-not-touch
     value))

  (def-init-say "init" "info"))


;; Actual recompilation machinery.
(init-say "Running self-recompile check.." "recompile-check")
(defvar my--init-boot-important-flag-do-not-touch nil)
(let ((ok
       (my-load-may-compile-el
        (emacs-path "init")
        t
        (elispfiles-path "my-init-macros.el")
        package-user-dir ;; package.el installs packages here
        )))
  (cond

   ;; Recompiling, set flag to prevent loading begin.el twice.
   ((and (numberp ok) (= 1 ok))
    (progn
      (init-say
       "From old init.elc: init.elc was outdated or deps changed; recompiled.."
       "recompile-check"
       "old-init")
      (setq my--init-boot-important-flag-do-not-touch "old-init")))

   ;; Everything is ok, carry on.
   ((and (numberp ok) (= 0 ok))
      (setq my--init-boot-important-flag-do-not-touch "new-init"))

   ;; Failure, set flag to prevent loading anything further.
     (t
      (progn
        (error
         "Error in init.el: self-compile failed: %s" ok)
        (setq my--init-boot-important-flag-do-not-touch "initfail")))))

;;; -------------------------------------------------- ;;;
;;; GUARD AGAINST EVALUATING TWICE                     ;;;
;;; -------------------------------------------------- ;;;


;;; Actual init logic:
(when (my--init-boot-important-flag-is "new-init")
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

;;; -------------------------------------------------- ;;;
;;; CUSTOMIZE AND INITSPLIT                            ;;;
;;; -------------------------------------------------- ;;;


;; set custom file name
(setq custom-file (customfiles-path "custom.el"))

(if (file-exists-p (elispfiles-path "initsplit/initsplit.el"))
    (let ((ok (my-load-may-compile-el
               (elispfiles-path "initsplit/initsplit")
               t)))
      (cond
       ;; Recompiling it.
       ((and (numberp ok) (= 1 ok))
        (init-say "initsplit.el changed; recompiled.." "customize-setup"))
       ;; Recompilation not needed. Just load the file.
       ((and (numberp ok) (= 0 ok))
        (load (elispfiles-path "initsplit/initsplit.elc")))
       ;; Uh oh.
       (t
        (error
         "Error in init.el: initsplit.el compilation failed: %s" ok))))
  (init-say (format "'%s' not found (git submodule not fetched?)"
                    (elispfiles-path "initsplit/initsplit.el"))
            "customize-setup")
  (init-say "Customize won't load/save settings appropriately" "warning"))




;;; -------------------------------------------------- ;;;
;;; LOAD LITERATE INIT                                 ;;;
;;; -------------------------------------------------- ;;;


(eval-when-compile
  (defsubst load-literate-initfile()
    (let* ((initfile (emacs-path "lit-init"))
           (init_org (concat initfile ".org"))
           (init_el  (concat initfile ".el"))
           (init_elc (concat init_el "c"))
           (pkg-dir-mod-time
            (nth 5 (my--init-file-attributes package-user-dir))))
      (unless (file-exists-p init_org)
        (error (format "FATAL: literate init file '%s' not found!" init_org)))
      (if (and
           ;; We're checking three things here:
           ;;
           ;; 1. The .elc file exists.
           ;; 2. The .elc file is not older than the pkg dir.
           ;; 3. The .elc file is not older than the .org file it was
           ;;    generated from.
           (file-exists-p init_elc)
           (my--init-newer-than-p init_elc pkg-dir-mod-time)
           (my--init-newer-file-p init_elc init_org))

          ;; It's neither missing nor outdated? Just load it then.
          (load-file init_elc)

        ;; Else: Rebuild!
        (init-say (format "Need to rebuild %s" init_elc) "recompile-lit-init")

        ;; Tangle, if .org is newer than .el or .el is missing.
        (when (or (not (file-exists-p init_el))
                  (my--init-newer-file-p init_org init_el))
          (require 'ob-tangle) ; in scope, if package-initialize did its job
          (init-say
           (format "Generated %s"
                   ;; Passing an output path is strictly speaking
                   ;; superflous, since lit-init.org sets :tangle to
                   ;; yes globally. But I'm passing init_el in here
                   ;; anyway, just to be explicit.
                   (car (last (org-babel-tangle-file
                               init_org
                               init_el
                               "emacs-lisp"))))
           "recompile-lit-init"))

        ;; Compile the relevant .el file.
        (init-say
         (format "Compiled and loaded %s"
                 (progn
                   (byte-compile-file init_el t)
                   init_el))
         "recompile-lit-init")))))

;; Do it.
(load-literate-initfile)


;;; -------------------------------------------------- ;;;
;;; LOAD CUSTOM FILE                                   ;;;
;;; -------------------------------------------------- ;;;


;; load custom vars
(load custom-file)

;; Assuming initsplit loaded correctly, we should load all those split
;; custom files.
(when (and (boundp 'initsplit-default-directory)
           (boundp 'initsplit-customizations-alist))
  (mapc #'(lambda (initsplit-entry)
            (let* ((el-path (customfiles-path (cadr initsplit-entry)))
                   (elc-path (concat el-path "c")))
              (if (file-exists-p elc-path)
                  (load elc-path)
                (load el-path))))
        initsplit-customizations-alist))



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

(when (my--init-boot-important-flag-is "old-init")
  (init-say
   "Skipped remaining init steps; new init.elc should've covered those.."
   nil
   "old-init"))
;;; End of Actual init logic

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
