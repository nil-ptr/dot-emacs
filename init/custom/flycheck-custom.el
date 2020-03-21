(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-check-syntax-automatically
   (quote
    (save new-line mode-enabled)))
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoctor asciidoc bazel-buildifier c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint emacs-lisp emacs-lisp-checkdoc erlang-rebar3 erlang eruby-erubis eruby-ruumba fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-staticcheck groovy haml handlebars haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json json-jq jsonnet less less-stylelint llvm-llc lua-luacheck lua markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc protobuf-prototool pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar terraform terraform-tflint tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby yaml-yamllint)))
 '(flycheck-clang-args
   (quote
    ("-fPIE")))
 '(flycheck-clang-include-path nil)
 '(flycheck-clang-language-standard "c++11")
 '(flycheck-display-errors-function
   (quote flycheck-pos-tip-error-messages))
 '(flycheck-gcc-include-path nil)
 '(flycheck-ghc-args
   (quote
    ("-v")))
 '(flycheck-ghc-language-extensions nil)
 '(flycheck-ghc-no-user-package-database nil)
 '(flycheck-ghc-package-databases nil)
 '(flycheck-ghc-search-path nil)
 '(flycheck-global-modes
   (quote
    (not org-agenda-mode org-mode)))
 '(flycheck-haskell-ghc-executable "/opt/ghc/bin/ghc")
 '(flycheck-haskell-runghc-command
   (quote
    ("/opt/ghc/bin/runghc" "-i")))
 '(flycheck-hlint-ignore-rules nil)
 '(flycheck-hlintrc nil)
 '(flycheck-pos-tip-timeout 5))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error
   ((t
     (:underline
      (:color "red" :style wave)))))
 '(flycheck-info
   ((t
     (:underline
      (:color "lime green" :style wave))))))
