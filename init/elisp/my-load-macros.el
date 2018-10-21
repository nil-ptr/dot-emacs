;;; my-load-macros.el --- elisp macros used by my init.el (and friends) to compile and load/require literate elisp .org files.  -*- lexical-binding: t; -*-

(defmacro my-load-age-of-file (path)
  "[INTERNAL]"
  `(float-time
   (time-subtract
    (current-time)
    (nth 5
         (or (file-attributes (file-truename ,path))
             (file-attributes ,path))))))

(defmacro my-load-is-more-recent (derived-file original-file)
  "[INTERNAL]"
  `(and (file-exists-p ,derived-file)
        (> (my-load-age-of-file ,original-file)
           (my-load-age-of-file ,derived-file))))

(defmacro my-load-maybe-compile-el (path)
  "Compile the PATH.el if PATH.elc doesn't exist or is outdated."
  `(let ((file-name (concat ,path ".el"))
          (compiled-name (concat ,path ".elc")))
     (unless (my-load-is-more-recent compiled-name file-name)
       (message "%s %s"
                (progn (byte-compile-file file-name)
                       "Compiled .el file:")
                compiled-name))))

(defmacro my-load-maybe-compile-org (path)
  "Tangle and compile PATH.org if PATH.elc doesn't exist or is outdated."
  `(let ((org-name (concat ,path ".org"))
         (exported-name (concat ,path ".el"))
         (compiled-name (concat ,path ".elc")))
     (unless (my-load-is-more-recent compiled-name org-name)
       (progn
         (setq exported-name
               (car (last (org-babel-tangle-file
                           org-name
                           exported-name
                           "emacs-lisp"))))
         (message "%s %s"
                  (progn (byte-compile-file exported-name)
                         "Compiled .org file: ")
                  compiled-name)))))

(provide 'my-load-macros)
;;; my-load-macros.el ends here
