;;; my-load-macros.el --- elisp macros used by my init.el (and friends) to compile and load/require literate elisp .org files.  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Just some scaffolding for my init.el file.

;;; Code:


;; Timestamp checking functions.

(defmacro my-load-is-newer-than (file &rest other-files)
  "Return t if FILE is newer than all entries in OTHER-FILES."
  `(if
       (and
        ,@(mapcar
           (lambda (f)
             `(time-less-p (nth 5
                                (or (file-attributes (file-truename ,f))
                                    (file-attributes ,f)))
                           (nth 5
                                (or (file-attributes (file-truename ,file))
                                    (file-attributes ,file)))))
           other-files))
       t))

(defmacro my-load-is-older-than (file &rest other-files)
  "Return t if FILE is older than all entries in OTHER-FILES."
  `(if
       (and
        ,@(mapcar
           (lambda (f)
             `(time-less-p (nth 5
                                (or (file-attributes (file-truename ,file))
                                    (file-attributes ,file)))
                           (nth 5
                                (or (file-attributes (file-truename ,f))
                                    (file-attributes ,f)))))
           other-files))
       t))

(defsubst my--init-file-attributes(file)
  "Get the file attributes of FILE.

Checks the result of `file-truneame' before FILE itself."
  (or (file-attributes (file-truename file))
      (file-attributes file)))

(defsubst my--init-newer-file-p (fileA fileB)
  "Return t if FILEA is newer than FILEB."
  (time-less-p (nth 5 (my--init-file-attributes fileB))
               (nth 5 (my--init-file-attributes fileA))))

(defsubst my--init-newer-than-p (file time)
  "Return t if FILE was modified after TIME."
  (time-less-p time
               (nth 5 (my--init-file-attributes file))))

(defsubst my--init-newer-than-all (file &rest other-files)
  "Return true if FILE is older than all OTHER-FILES."
  (and
   (mapcar
    #'(lambda (f)
        (my--init-newer-file-p f file))
    other-files)))


(defmacro my-load-check-org-elc-freshness (files timeout)
  "Check freshness of generated elc files indicated by FILES.

Input should be a list of files WITHOUT EXTENSIONS.  The list is
filtered, retaining those entries for which one of the following is true:

1. The .org file is newer than the .elc file.
2. The .elc file does not exist.
3. The .elc was last modified before TIMEOUT."
  `(seq-filter
    #'(lambda (file)
        (or
         (not (file-exists-p (concat file ".elc")))
         (not (my--init-newer-than-p (concat file ".elc") ,timeout))
         (my--init-newer-file-p (concat file ".org")
                                (concat file ".elc"))))
    ,files))


;; Tangle-and-compile functions.

(defmacro my-load-may-compile-el (path load &rest other-files)
  "Compile PATH.el if needed, and load if we did and LOAD was non-nil.

Recompilation is considered to be needed if PATH.elc is older
than PATH.el or any of the OTHER-FILES.  Returns 0 if the file
was up to date, 1 if it was recompiled (and possibly loaded) or
nil on error."
  (let ((file (concat path ".el"))
        (compiled-file (concat path ".elc")))
    `(if (not
          (and
           (file-exists-p ,compiled-file)
           (my--init-newer-than-all ,compiled-file ,file ,@other-files)))

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

(provide 'my-load-macros)
;;; my-load-macros.el ends here
