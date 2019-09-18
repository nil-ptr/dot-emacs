;;; my-init-macros.el --- elisp macros used in my init.el  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Just some scaffolding for my init.el file.

;;; Code:

;; Path helpers


(defsubst emacs-path(name)
  (expand-file-name name user-emacs-directory))
(defsubst elispfiles-path(name)
  (expand-file-name name my-init-elisp-dir))
(defsubst customfiles-path(name)
  (expand-file-name name my-init-custom-dir))

;; Logging helper

(defmacro def-init-say (prefix &optional default-category)
  "Define a logger with prefix PREFIX.

Optionally, a DEFAULT-CATEGORY may be specified.  This will be
used in calls to the resulting 'init-say' function, if no
category argument is passed in.  If DEFAULT-CATEGORY is omitted
or nil, and no category is given at a 'init-say' call site, no
category field will be displayed in the output.

Should only be called from within `eval-when-compile'."
  `(defsubst init-say (msg &optional category override-prefix)
         (message (concat (format "[%s" (or override-prefix ,prefix))
                          (if (or category ,default-category)
                              (format "|%s] " (or category ,default-category))
                            "] ")
                          "%s") msg)))


;; Timestamp checking functions.


(defsubst my--init-file-attributes (file)
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



(defsubst my-load-check-org-elc-freshness (files timeout)
  "Check freshness of generated elc files indicated by FILES.

Input should be a list of files WITHOUT EXTENSIONS.  The list is
filtered, retaining those entries for which one of the following is true:

1. The .org file is newer than the .elc file.
2. The .elc file does not exist.
3. The .elc was last modified before TIMEOUT."
  (seq-filter
   #'(lambda (file)
       (or
        (not (file-exists-p (concat file ".elc")))
        (not (my--init-newer-than-p (concat file ".elc") timeout))
        (my--init-newer-file-p (concat file ".org")
                               (concat file ".elc"))))
   files))


;; Tangle-and-compile functions.

(defsubst my-load-may-compile-el (path load &rest other-files)
  "Compile PATH.el if needed, and load if we did and LOAD was non-nil.

Recompilation is considered to be needed if PATH.elc is older
than PATH.el or any of the OTHER-FILES.  Returns 0 if the file
was up to date, 1 if it was recompiled (and possibly loaded) or
nil on error.

Warning: Ensure that file in OTHER-FILES actually exists before
calling. Missing files will be perceived as older than PATH.elc."
  (let ((file (concat path ".el"))
        (compiled-file (concat path ".elc")))
    (if (not
          (and
           (file-exists-p compiled-file)
           (seq-every-p #'(lambda (f)
                            (or
                             (not (file-exists-p f))
                             (my--init-newer-file-p compiled-file f)))
                        (if other-files
                            (cons file other-files)
                          (list file)))))

         (if (byte-compile-file file load)
             (progn
               (message "%s %s"
                        "Compiled and Loaded"
                        file)
               1)
           (progn (message "bytecomp failed %s %s" file load) nil))
       0)))


;;;

(provide 'my-init-macros)
;;; my-init-macros.el ends here
