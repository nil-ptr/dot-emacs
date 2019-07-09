;;; my-init-macros.el --- elisp macros used in my init.el  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Just some scaffolding for my init.el file.

;;; Code:

(defsubst unbound-p (symbol)
  "Return t if SYMBOL isn't bound."
  (not (boundp symbol)))

(defmacro bound-and-nil-p (symbol)
  "Return t if SYMBOL is bound to nil."
  `(and (boundp (quote ,symbol))
        (eq nil ,symbol)))



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
             (cons file other-files))))

         (if (byte-compile-file file load)
             (progn
               (message "%s %s"
                        "Compiled and Loaded"
                        file)
               1)
           (progn (message "bytecomp failed %s %s" file load) nil))
       0)))


;;;

(defmacro my-init-pre-compile (&rest body)
  "Pre compilation phase of init, treats BODY like `progn'.

Should not contain anything that doesn't need to be here. In
particular, the `my-init-finally-block' should run the same
regardless of whether this block was executed from the same
version of the file or not. Which is all to say, don't spread
compile-time logic across blocks.

This block always runs, meaning the inner one will \"overwrite\"
the \"outer\" one, should they happen to set the same variable."
  `(progn
     (defvar my-init-self-compile-indicator-var t)
     ,@body
     (setq my-init-self-compile-indicator-var nil)))

(defmacro my-init-load-post-compile (&rest body)
  "Post compile/load phase of init, treats BODY like `progn'.

Note that this is skipped if compile/load is skipped."
  `(when (bound-and-true-p my-init-self-compile-indicator-var)
     ,@body))

(defmacro my-init-finally-body (&rest body)
  "Unconditional block, treats BODY like `progn'.

This will always run, and in particular will always use the
\"inner\" definition. That is, if we're compiling the finally
block in the file being compiled will run, and the block in the
file preforming the compilation will be ignored."
  `(when (or (bound-and-nil-p my-init-self-compile-indicator-var)
             (unbound-p my-init-self-compile-indicator-var))
     ,@body))

(defsubst emacs-path(name)
  (expand-file-name name user-emacs-directory))
(defmacro initfiles-path(name)
  `(expand-file-name ,name my-init-files-dir))
(defsubst elispfiles-path(name)
  (expand-file-name name my-init-elisp-dir))
(defmacro caskfiles-path(name)
  `(expand-file-name ,name my-init-cask-pkg-dir))


(defmacro def-init-say (prefix)
  "Should only be called from within `eval-when-compile'."
    `(defsubst init-say (msg &optional category)
       (message (concat ,(format "[%s" prefix)
                         (if category (format "|%s] " category) "] ")
                         "%s") msg)))

(defmacro init-say-s (msg &rest args)
  `(message "[%s] %s"
            (eval-when-compile (file-name-nondirectory
                                (or load-file-name "placeholder")))
		       ,msg))



(provide 'my-init-macros)
;;; my-init-macros.el ends here
