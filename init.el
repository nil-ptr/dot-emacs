;;; init ---  Not my real init file. Just a tribute. -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; This exists to recompile itself and the actual init file: begin.el
;;;
;;; See CHANGELOG.md for changes.

;;; Code:

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
              (concat proto "://elpa.gnu.org/packages/")))))
   ;; Return the arrived at list.
   package-archives))

(eval-and-compile
  (package-initialize)
  (defconst my-init-elisp-dir (expand-file-name "init/elisp"
                                                user-emacs-directory))
  (add-to-list 'load-path my-init-elisp-dir))
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
      (init-say "permission not granted; init success doubtful.."))))

(eval-when-compile
  (defmacro my-init-boot-recompile-w-file-watch (file &rest watch-files)
    `(my-load-may-compile-el
      ,(emacs-path file)
      t
      ,@watch-files))

  (defsubst my--bound-and-eq (v value)
    (and (boundp v) (eq v value))))

;(message "Starting timer for the loading of %s..." load-file-name)
;(defconst emacs-start-time (current-time))
(eval-when-compile (message "%s" load-path))
(defvar my--init-boot-important-flag-do-not-touch nil)
(let ((ok
       (my-init-boot-recompile-w-file-watch
        "init"
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

;; Don't bother to continue loading if the flag isn't t.
(when (my--bound-and-eq my--init-boot-important-flag-do-not-touch t)
  (let ((ok (my-init-boot-recompile-w-file-watch
             "begin"
             (elispfiles-path "my-load-macros.el")
             (emacs-path "elpa") ;; package.el installs packages here
             ;(caskfiles-path "elpa")
             )))
    (cond
     ;; Recompiling, set flag to prevent loading begin.el twice.
     ((and (numberp ok) (= 1 ok))
      (progn
        (init-say
         "(old): begin.elc was outdated or deps changed; recompiled")
        (setq my--init-boot-important-flag-do-not-touch 'begin)))
     ;; Everything is ok, carry on.
     ((and (numberp ok) (= 0 ok))
      (setq my--init-boot-important-flag-do-not-touch t))
     ;; Failure, set flag to prevent loading anything further.
     (t
      (progn
        (error
         "Error in init.el: begin.el re-compile failed: %s" ok)
        (setq my--init-boot-important-flag-do-not-touch 'beginfail))))))

(when (my--bound-and-eq my--init-boot-important-flag-do-not-touch t)
  (init-say "Loading begin.elc normally..")
  (load-file (emacs-path "begin.elc"))
  ;; Signal that it's loaded.
  (setq my--init-boot-important-flag-do-not-touch 'begin))


(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
