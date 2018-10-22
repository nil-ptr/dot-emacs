;;; init ---  Not my real init file. Just a tribute. -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; This exists to recompile itself and the actual init file: begin.el
;;;
;;; See CHANGELOG.md for changes.

;;; Code:

;; This needs to be here to please package.el
;(package-initialize)

(eval-and-compile
  (defconst my-init-files-dir (expand-file-name "init" user-emacs-directory))
  (defconst my-init-elisp-dir (expand-file-name "elisp" my-init-files-dir))
  (defconst my-init-cask-pkg-dir (expand-file-name
                                  ".cask/25.1"
                                  user-emacs-directory))
  (add-to-list 'load-path my-init-files-dir)
  (add-to-list 'load-path my-init-elisp-dir))

(eval-when-compile
  (require 'my-load-macros)
  (require 'my-init-macros)

  (defmacro my-init-boot-recompile-w-file-watch (file &rest watch-files)
    `(my-load-may-compile-el
      ,(emacs-path file)
      t
      ,@watch-files)))

;(message "Starting timer for the loading of %s..." load-file-name)
;(defconst emacs-start-time (current-time))
(defvar my-init-boot-important-flag-do-not-touch t)
(let ((ok
       (my-init-boot-recompile-w-file-watch
        "init"
        (elispfiles-path "my-init-macros.el")
        (elispfiles-path "my-load-macros.el"))))
  (cond ((and (numberp ok) (= 1 ok))
         (progn
           (init-say
            "(old): init.elc was outdated or deps changed; recompiled")
           (setq my-init-boot-important-flag-do-not-touch nil)))
         ;; carry on as usual
        ((and (numberp ok) (= 0 ok)))
        (t
         (progn
           (error "Error in init.el: self-compile failed; aborting")
           (setq my-init-boot-important-flag-do-not-touch nil)))))

;; Don't bother to continue loading if the flag isn't t.
(when (bound-and-true-p my-init-boot-important-flag-do-not-touch)
  (let ((ok (my-init-boot-recompile-w-file-watch
            "begin"
            (elispfiles-path "my-load-macros.el")
            (caskfiles-path "elpa"))))
    (cond ((and (numberp ok) (= 1 ok))
           (progn
             (init-say
              "(old): begin.elc was outdated or deps changed; recompiled")
             (setq my-init-boot-important-flag-do-not-touch nil)))
          ;; carry on as usual
          ((and (numberp ok) (= 0 ok)))
          (t
           (progn
             (error "Error in init.el: begin.el re-compile failed; aborting")
             (setq my-init-boot-important-flag-do-not-touch nil))))))

(when (bound-and-true-p my-init-boot-important-flag-do-not-touch)
  (init-say "Loading begin.elc normally..")
  (load-file (emacs-path "begin.elc")))

(provide 'init)
;;; init.el ends here
