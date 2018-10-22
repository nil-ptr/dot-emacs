;;; my-load-macros.el --- elisp macros used by my init.el (and friends) to compile and load/require literate elisp .org files.  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Just some scaffolding for my init.el file.

;;; Code:


(defmacro my-load-file-last-modified-time (path)
  "Determine when PATH was last modified-time."
  `(nth 5
         (or (file-attributes (file-truename ,path))
             (file-attributes ,path))))

(defmacro my-load-is-newer-than (file &rest other-files)
  "Return t if FILE is newer than all entries in OTHER-FILES."
  `(if
       (and
        ,@(mapcar
           (lambda (f)
             `(time-less-p (my-load-file-last-modified-time ,f)
                           (my-load-file-last-modified-time ,file)))
           other-files))
       t))

(defmacro my-load-is-older-than (file &rest other-files)
  "Return t if FILE is older than all entries in OTHER-FILES."
  `(if
       (and
        ,@(mapcar
           (lambda (f)
             `(time-less-p (my-load-file-last-modified-time ,file)
                           (my-load-file-last-modified-time ,f)))
           other-files))
       t))

(defmacro my-load-may-compile-el (path load &rest other-files)
  "Compile PATH.el if needed, and load if we did and LOAD was non-nil.

Recompilation is considered to be needed if PATH.elc is older
than PATH.el or any of the OTHER-FILES. Returns 0 if the file was
up to date, 1 if it was recompiled (and possibly loaded) or nil
on error."
  (let ((file (concat path ".el"))
        (compiled-file (concat path ".elc")))
    `(if (not
          (and
           (file-exists-p ,compiled-file)
           (my-load-is-newer-than ,compiled-file ,file ,@other-files)))

         (if (byte-compile-file ,file ,load)
             (progn
               (message "%s %s"
                        "Compiled and Loaded"
                        ,file)

               1)
           nil)
       0)))

(defmacro my-load-may-compile-load-el (path)
  "Load PATH.elc, compiling PATH.el if needed.

By needed I mean if PATH.elc is missing or older than PATH.el."
  (let ((file (concat path ".el"))
        (compiled-file (concat path ".elc")))
    `(if (or (not (file-exists-p ,compiled-file))
             (my-load-is-older-than ,compiled-file ,file))
         (message "%s %s"
                  (progn
                    (byte-compile-file ,file t)
                    "Compiled and Loaded")
                  ,file)
       (load-file ,compiled-file))))


(defmacro my-load-may-compile-load-el-list (&rest paths)
  "Apply `my-load-may-compile-load-el' to path in PATHS."
  `(progn
     ,@(mapcar (lambda (x)
                 `(my-load-may-compile-load-el ,x))
               paths)))

(defmacro my-import-el(&rest paths)
  "Alias for `my-load-may-compile-load-el-list'."
  `(my-load-may-compile-load-el-list ,@paths))


(defmacro my-load-may-compile-load-org (path)
  "Load PATH.elc, compiling and tangling PATH.org if needed.

By needed I mean if PATH.elc is missing or older than PATH.org.

This macro assumes that `org-babel-tangle-file' (defined in
`ob-tangle.el') is in scope."
  (let ((file (concat path ".org"))
        (exported-file (concat path ".el"))
        (compiled-file (concat path ".elc")))
    `(if (not
          (and
           (file-exists-p ,compiled-file)
           ;; We could also compare against ,exported-file here, but I
           ;; really only care if it's older than the .org file, since
           ;; the .el is only an intermediate file.
           (my-load-is-newer-than ,compiled-file ,file)))
         (progn
           (let ((new-file-name
                  (car (last (org-babel-tangle-file
                              ,file
                              ,exported-file
                              "emacs-lisp")))))

           (message "%s %s"
                    (progn
                      (byte-compile-file new-file-name t)
                      "Compiled and Loaded")
                    new-file-name)))
       (load-file ,compiled-file))))

(defmacro my-load-may-compile-load-org-list (&rest paths)
  "Apply `my-load-may-compile-load-org' to path in PATHS.

This macro assumes that `org-babel-tangle-file' (defined in
`ob-tangle.el') is in scope."
  `(progn
     ,@(mapcar (lambda (x)
                 `(my-load-may-compile-load-org ,x))
               paths)))

(defmacro my-import-org(&rest paths)
  "Alias for `my-load-may-compile-load-org-list'.

This macro assumes that `org-babel-tangle-file' (defined in
`ob-tangle.el') is in scope."
  `(my-load-may-compile-load-org-list ,@paths))


;testcode
;(if (my-load-is-newer-than "~/.Xmodmap" ("~/.bash_aliases" "~/.profile"))
;   (message "newer")
;  (message "older"))

(provide 'my-load-macros)
;;; my-load-macros.el ends here
