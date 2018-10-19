;;; init --- My init file. Duh.
;;; Commentary:
;;; Init file.  Created 2014-05-19
;;; Updated 2018-10-18
;;;   - Added some elisp to compute how long it took to load this
;;;     file, and it's dependencies.  Idea taken from jwiegley's
;;;     dotfile Rep.
;;;   - Removed some further cruft in the file, and cleaned it up.
;;;   - Changed how the imports of lit-init files works. I now
;;;     manually check that whether there are any .elc files, and
;;;     whether those are newer than the corresponding .org files. If
;;;     yes: load the .elc, if no: tangle, compile and load the .org
;;;     file. This is slow because, in addition to tangling and
;;;     byte-compiling, we also end up having to load the org package
;;;     early so that we can use org-tangle-file. Normally, org is
;;;     loaded lazily. The speed up when all .elc files are present is
;;;     noticable, however.
;;; Updated 2018-10-17
;;;   - I've migrated almost all of the contents of this file to a
;;;     series of literate elisp files.  I've also switched to a more
;;;     principled use of use-package rather than require to reduce
;;;     load times.  This has vastly improved startup time, despite the
;;;     fact that code has to be extracted from literate elisp/org
;;;     files on the fly.  Lazy loading makes a massive difference.
;;;   - Moved the customize stuff to a separate file, to prepare for
;;;     splitting this increasingly gargantuan monolith of an
;;;     initialisation file into smaller, more managable parts.
;;;   - Deleted an unneccesary 'exec-path modification that added a
;;;     path that was already avaliable trough the PATH environment
;;;     variable.
;;; Updated 2018-10-15
;;;   - Got rid of the "H-p" smartparens-strict-mode keybinding, to
;;;     free up that combination for projectile.  Using "s-p" didn't
;;;     feel quite right to me.
;;; Updated 2018-09-27
;;;   - Added an explicit key prefix binding for the projectile key map,
;;;     since it no longer exports a default one.
;;; Updated 2018-06-20
;;;   - Added functions for opening the ;; and ;W agenda views.
;;;     This lets me open those by default.
;;; Updated 2018-06-20
;;;   - Removed some code at the end of this file, having to do with agda mode.
;;; Updated 2017-09-12
;;;   - We no longer load org-bullets manually.
;;;   - Also adds hlint-refactor-mode
;;; Updated 2017-05-29
;;;   - Had to get rid of org-git-link as it was screwing with org-store-link.
;;;   - Also installed org-id
;;; Updated 2017-05-21
;;;   - Upgraded to emacs-25.2, and had to get rid of rust-flycheck.
;;; Updated 2017-05-06
;;;   - Adds SC again.
;;;   - Adds a keybinding for org-store-link
;;; Updated 2017-04-19
;;;   - Added org as a cask source, so that I could get my hands
;;;     on the org-contrib modules.  Specifically org-git-link.
;;; Updated 2017-04-15
;;;   - Removed my hydralisk thing.  I don't use it much
;;;     anymore.
;;; Updated 2016-10-07
;;;   - Removed global bindings for org-agenda-list
;;;     and org-todo-list
;;;   - Added binding "H-a" for the more general
;;;     org-agenda
;;; Updated 2016-09-13
;;;  Added shortcuts:
;;;   - "H-a" for org-agenda-list
;;;   - "H-t" for org-todo-list
;;; Updated 2016-07-13
;;;  Added auto yas again.
;;; Updated 2016-06-08
;;;  Added:
;;;   - intero-mode
;;;   - whitspace-cleanup-mode
;;;   - column-enforce-mode
;;;   - highlight-indentation-mode
;;;  Removed:
;;;   - Reliance on hi2 and haskell-interactive
;;;  Updated my hydra defs
;;; Updated 2016-04-13
;;;  Added my own win-split function.  Because lazy.
;;; Updated 2016-02-06
;;;  Added buf-move
;;;  Added global key bindings for it, and
;;;  windmove.
;;; Updated 2015-12-07
;;;  Removed SC.  I never use it.
;;;  Added helm-ag.
;;; Updated 2015-11-28
;;;  Removed helm-swoop bindings.
;;;  I find helm-occur more useful as a search tool.
;;;  Added encourage-mode.
;;;  Some minor cleanup.
;;; Updated 2015-09-09
;;;  Removed the racer hacks. This program should work
;;;  without them now, from what I'm told.
;;; Updated 2015-07-03
;;;  Commented out a line diminishing magit-auto-revert.
;;; Updated 2015-06-08
;;;  Removed auto-yasnippet
;;;  These days I tend to just write temporary
;;;  snippets manually. I tend to make fewer mistakes
;;;  this way.
;;; Updated 2015-05-24
;;;  Swapped ace for avy
;;;  Helm-Swoop bindings
;;;  Turned off speedbar
;;; Updated 2015-05-24
;;;  Added ace-jump stuff:
;;;   - ace-jump
;;;   - ace-isearch
;;;   - ace-zap
;;;  Added a keybinding for save-buffers-kill-emacs
;;; Updated 2015-05-09
;;;  Dropped sublimity.  Was too slow to be useful.
;;; Updated 2015-04-30
;;;  Added sublimity for minimap
;;; Updated 2015-04-28
;;;  Added some more minor modes to
;;;  diminished
;;; Updated 2015-04-26
;;;  Switched off nyan by default
;;; Updated 2015-04-23
;;;  Added a few minor modes to diminish:
;;;   - flyspell
;;;   - yas
;;;   - SP
;;;   - magit auto revert
;;;  Some minor cleanup
;;;  Swapped mapcar for mapc in a function below
;;;   since my lint was complaining
;;;  Disabeled the magit warning
;;; Updated 2015-04-22
;;;  Added require projectile
;;;  and activated it's use as a global mode
;;;  Added Helm-projectile mode
;;;  Added projectile speedbar mode
;;;  Added more git plugins
;;;  Added powerline
;;; Updated 2015-04-17
;;;  Some minor cleanup
;;; Updated 2015-02-21
;;;  Added some stuff related to flyspell
;;; Updated 2015-01-27
;;;  Added som pandoc stuff.
;;; Updated 2015-01-14
;;;  Added helm + helm-company + helm-hoogle
;;;  Changed some company keybinds
;;; Updated 2015-01-11
;;;  Having cabal issues.  ghc-mod is out for now
;;; Updated 2014-12-19
;;;  Changed several bindings in haskell-mode
;;;   plus ghc-mod
;;;  Changed the global dabbrev-expand binding
;;; Updated 2014-12-15
;;;  Added function (stolen from internets) for
;;;   showing yasnippets with popup.el
;;; Updated 2014-12-14
;;;  Trying out strict delimter mode as default
;;;  ... -> Decided I prefer regular smartparens
;;;  Added keybinding for dabbrev-expand
;;; Updated 2014-12-13
;;;  Added keybindings for
;;;  window resizing
;;; Updated 2014-12-10
;;;  Switched off ghc-mod; I really only want ghc-comp-init
;;;  Some additional clean up
;;; Updated 2014-12-06
;;;  Added a (pallet-mode t) line.
;;;  Switched from AC to company
;;;  Installed ghc-mod
;;; Updated 2014-12-05
;;;  Some clean up
;;; Updated 2014-10-7
;;;  Now relies on Cask for most of the package management.
;;;  Some things still need explicit dealing with though.


;;; Code:


;; Init load timing


;; This needs to be here to please package.el
;(package-initialize)

(message "Starting timer for the loading of %s..." load-file-name)
(defconst emacs-start-time (current-time))


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

;; set custom file name
(setq custom-file "~/.emacs.d/custom.el")

;; Load use-package
(require 'use-package)
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;; Load (lazily) org
(use-package org
  :defer t
  :defines (org-agenda-mode-map)
  :commands (org-store-link
             org-agenda-mode-map
             org-babel-tangle-file
             org-agenda)
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
(defun nilgu/init/compile-or-load-lit-init (name)
  "Load, or if needed, compile the init import given by NAME. docs
tbd"
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
      ;; The org file is newer => compile and load
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
(mapc 'nilgu/init/compile-or-load-lit-init lit-emacs-init-imports)


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
