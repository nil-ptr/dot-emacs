;;; my-init-macros.el --- elisp macros used in my init.el  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Just some scaffolding for my init.el file.

;;; Code:

(defmacro unbound-p (symbol)
  "Return t if SYMBOL isn't bound."
  `(not (boundp (quote ,symbol))))

(defmacro bound-and-nil-p (symbol)
  "Return t if SYMBOL is bound to nil."
  `(and (boundp (quote ,symbol))
        (eq nil ,symbol)))

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
(defmacro elispfiles-path(name)
  `(expand-file-name ,name my-init-elisp-dir))
(defmacro caskfiles-path(name)
  `(expand-file-name ,name my-init-cask-pkg-dir))


(defmacro init-say (msg &rest args)
  `(message ,(concat "[%s] " msg)
           (file-name-nondirectory load-file-name)
           ,@args))




(provide 'my-init-macros)
;;; my-init-macros.el ends here
