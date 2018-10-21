;;; init --- My init file. Duh.
;;; Commentary:
;;;
;;; Nothing much to say here.
;;;
;;; See CHANGELOG.md for changes.

;;; Code:


;; Init load timing


;; This needs to be here to please package.el
;(package-initialize)

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



;; Activate cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(add-to-list 'load-path "~/.emacs.d/.cask/")

;; include pallet
(require 'pallet)
(pallet-mode t)

;; Fixes a potential keyboard issue.
(require 'iso-transl)

;; set custom file name
(setq custom-file "~/.emacs.d/custom.el")

;; Load use-package
;(eval-when-compile
(require 'use-package);)
(require 'diminish)
(require 'bind-key)

;; (if init-file-debug
;;     (setq use-package-verbose t
;;           use-package-expand-minimally nil
;;           use-package-compute-statistics t
;;           debug-on-error t)
;;   (setq use-package-verbose nil
;;         use-package-expand-minimally t))

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
(defconst lit-emacs-init-dir "~/.emacs.d/init")

;; Init imports to run
(defconst lit-emacs-init-imports
  '("general"     ;; The "misc" import, basically.
    "helm"        ;; Sets up the helm-* packages I use.
    "org"         ;; Some setup for org-mode.
    "linum"       ;; Some setup to make global-linum-mode behave.
    "templating"  ;; yas, aya, yatemplate setup.
    ))

;;; ------------------------------ ;;;
;;; LITERATE INIT IMPORT MACHINERY ;;;
;;; ------------------------------ ;;;


;; Import function for literate (org) init files
(defun my-init-compile-or-load-lit-init (name)
  "Load, or if needed, compile the init import given by NAME.

This function will expand the NAME to 'lit-emacs-init-NAME.org',
and look for it in the `lit-emacs-init-dir'. If that file is
found, the following happens:

1. We check whether there is a 'lit-emacs-init-NAME.elc file in
   the same directory.

2. If there is, and it has a more recent modification time than
   'lit-emacs-init-NAME.org', load it and return.

   If not, continue to step 3.

3. Tangle 'lit-emacs-init-NAME.org' using `org-babel-tangle-file'
   to create 'lit-emacs-init-NAME.el'.

4. Byte-compile 'lit-emacs-init-NAME.el', load the resulting
   'lit-emacs-init-NAME.elc' and return."
  (let* ((age (lambda (file)
        (float-time
         (time-subtract (current-time)
                (nth 5 (or (file-attributes (file-truename file))
                           (file-attributes file)))))))
         (base-name (expand-file-name
                     (concat "lit-emacs-init-" name) lit-emacs-init-dir))
         (file (concat base-name ".org"))
         (exported-file (concat base-name ".el"))
         (compiled-file (concat base-name ".elc")))

    ;; tangle if the Org file is newer than the compiled elisp file
    (if (and (file-exists-p compiled-file)
             (> (funcall age file) (funcall age compiled-file)))

        ;; compiled file exists and is newer than the org file
        ;; => load that
        (load-file compiled-file)
      ;; the org file is newer => compile and load
      (progn
        (setq exported-file
              (car (last (org-babel-tangle-file
                          file
                          exported-file
                          "emacs-lisp"))))
        (message "%s %s"
                 (progn (byte-compile-file exported-file 'Load)
                        "Compiled and Loaded")
                 compiled-file)))))

;; Import the required literate elisp files
(mapc 'my-init-compile-or-load-lit-init lit-emacs-init-imports)


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
(provide 'init)
;;; init.el ends here
