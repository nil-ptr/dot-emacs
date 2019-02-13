# Changelog

Tracks changes to my .emacs files. This is not really a "versioned"
project per se, so I'll only bother with a single component versioning
number.

## 3 - 2019-02-13


### Changed

- Multiple org related dependencies in my `Cask` file have been
  removed in favour of `org-plus-contrib`. Which is what I should have
  depended on to begin with.

- Some of the loading logic in `begin.el` has been rewritten. The new
  variant will hopefully load org a little lazier. This also reduced
  the number of macros from `my-load-macros.el`.

  This also made the logic for actually tangling and compiling my
  literate init files a little lazier.

  Note that this removed a `use-package` import of `org` in
  `begin.el`, which means some keybindings defined here have been
  moved to [init/lit-emacs-init-org.org].

  See also the section on removed items below, for a list of load/init
  related macros that were deleted as a result of this change.

- The idle timers that display my agenda have been changed a little
  bit. In particular they have been made less sensitive to whether
  there are *fully* visible frames available or not. And one of them
  has been removed, on account of it being redundant.


### Added

- A macro called `my-load-check-org-elc-freshness` has been added to
  `my-load-macros.el`. See the doc-string for an explanation of what
  it is intended to do.

- Added a keybinding for `delete-frame`.

- Added `my-forward-narrow-page` and `my-backward-narrow-page`, which
  work like their non-narrowed cousins, but which will widen before
  moving and then narrow to page on arriving. Keybindings added.

- Added a function called `my-org-narrow-to-top-level-tree` to
  [init/lit-emacs-init-org.org]. Does what it says. With a prefix
  argument, it narrows to the parent node instead. Bindings also
  added, naturally.
  
- Added two `org-capture` templates, and some related keybindings.

### Fixed

- The `my-load-is-newer-than` and `my-load-is-older-than` macros no
  longer refer to `my-load-file-last-modified-time`. Its definition
  has been inlined.

  This is listed under "Fixed" rather than "Changed" because the
  nested macro invocations were causing incorrect results to be
  reported.

- The logic in [init/lit-emacs-init-linum.org] responsible for
  preventing `global-linum-mode` from screwing up buffers that aren't
  compatible with it broke down. Possibly due to changes in load
  logic. This has been resolved by overriding both `linum-on` *and*
  `global-linum-mode` using `advice-add`.


### Removed

- The `my-load-file-last-modified-time` macro from `my-load-macros.el`
  has been removed, because it was no longer needed. The two other
  macros that referred to it have had the relevant code inlined into
  them.

- The following macros from `my-load-macros.el` have been removed on
  account of being unused and unneccesary after the changes to
  `begin.el`:

  - `my-load-may-compile-el-list`
  - `my-import-el`
  - `my-load-may-compile-load-org`
  - `my-load-may-compile-load-org-list`
  - `my-import-org`


## 2 - 2018-11-01

### Changed

- The macros I use to load files during init have been overhauled. And
  `init.el` has been shortened significantly. Most of the logic now
  lives in `begin.el` which `init.el` compiles and loads.

- The agenda idle timer logic has been simplified significantly. It
  now displays the agenda after 5 minutes, and updates it every two
  minutes after that, if it's still visible. It gives up after idling
  for 8 (I think it was) hours.

- Some behind the scenes changes have been made to keybinding logic in
  [init/lit-emacs-init-general.org] and to load logic in `general` and
  [init/lit-emacs-init-helm.org].

  In particular, a couple of `flyspell` keybindings have been
  explicitly deleted, because I find them useless and they overwrite
  default bindings for far more important things in `org`.

- The code blocks in [init/lit-emacs-init-linum.org] have been
  structurally rearranged somewhat.

- Some keybindings for the `org-agenda-mode` map that were previously
  established via a hook, have been moved to a `:bind` clause of a
  `use-package` invocation instead.

- A plethora of changes have happened in the `custom.el` file
  reflecting changes in settings. Mostly `org` agenda layout related.

  Some non-agenda highlights:

  - Clocktable defaults.

  - `yasnippet`s can now trigger recursively.

  - Reduced some of the default prettying that `org` does when opening
    files to reduce agenda load times. Also consolidated a few
    different work related files into one org file. Also in the
    interest of reducing load times.

### Added

- Added my own notion of "page delimiter / separator", and function
  similar to those defined in `page.el` to support using the feature.

  This includes counting lines, navigating to the next page, narrowing
  to page, inserting well formed page separators and so forth. See the
  relevant section of [init/lit-emacs-init-general.org] for details.

  Keybindings were added for these things as well, naturally.

- Several keybindings for narrowing commands, including special ones
  for the `org-mode` map.

- Keybindings related to the `org-mode` clock have been added.

- Two new `yasnippet` snippets added, for use in org-mode.

- `pcomplete` has been activated and added as a completion-at-point
  backend to company. In the process I also disabled the default
  fallback functions for `pcomplete` because they error when you call
  them. An automatic check has been added that detects if the
  offending defcustom variables are at their default values. If they
  are when `pcomplete` is loaded, they will automatically be replaced
  with non-broken equivalents.

  Also worth noting that I don't like the default fallback behaviour
  even when it's working properly, and so I've disabled it in
  `customize` as well. Still, it's nice to have the auto-check as a
  fallback against the broken defaults.

- A version check for `emacs` itself has been added to
  [init/lit-emacs-init-linum.org], which disables `linum` if `emacs`
  is v26.1 or higher, since built in line numbering was added in that
  version.


### Fixed

- An important fix for `org-all-archive-files` has been added. This
  function doesn't work properly for trees with :ARCHIVE: properties
  in v9.1.14. My version does. The issue is caused by a mistaken use
  of `eq` for string comparison. My overriding version of this
  function is identical to the original save for swapping one use of
  `eq` for `string=`.


- `pdf-tools` was loading a little bit too eagerly. The same trick
  displayed in the README for `use-package` is now employed in
  [init/lit-emacs-init-general.org.org] to make `pdf-tools` load
  properly, but not too eagerly.

## 1 - 2018-10-21

### Changed

- The `iso-transl` import has been moved to a `require` in [init.el].

- The loading of `yatemplate` is now triggered via an
  `eval-after-load` expression, instead of a hook.

- Added an `eval-when-compile` form around the `use-package` import of
  `org` in [init/lit-emacs-init-org.org]. `org` is already loaded in
  [init.el], so the duplicate import only needs to be around to make
  the byte-compiler happy.

- Turned some list items under the "Functions" subheading of the "Org"
  heading of [init/lit-emacs-init-org.org] into sub-subheadings.
  Easier to read, that way.

- Swapped out a *really* ugly hack involving key remapping in the
  "Main Setup" section of [init/lit-emacs-init-helm.org], for a much
  more sensible piece of `advice` based code. This code is now far
  shorter and more well behaved.

- Delayed the unconditional load of `helm` slightly: it now happens
  after 5 seconds of idle time, instead of 3.

- Added an `eval-after-load` expression to make `helm-projectile` load
  as soon as `projectile` does, in order to intercept the use of
  `projectile` keybindings just in time.

- The [init/lit-emacs-init-general.org] file has been reshuffled quite
  substantially. Most of this is purely structure/readability related,
  however. Actual changes are detailed below:

  - Switched from direct `global-set-key` usage to the `bind-key`
    macro to set up my global keybindings.

  - Some changes were made to how company is loaded. It now loads
    unconditionally after 2 seconds, or if "C-:" (`company-complete`)
    is invoked. That binding has been moved to the global key map, in
    case I need to force this package to load **immediately** for
    whatever reason.

  - Slight changes to which modes I hook `column-enforce-mode` and
    `highlight-indentation-mode` on to.


### Added

- Some new `yasnippet` related keybindings in
  [init/lit-emacs-init-templating.org].

- Added a function to display either my work or personal agenda
  depending on the current time and day.

- Added idle timer based code to [init/lit-emacs-init-org.org], to
  show and update agenda views when `emacs` is idle for significant
  periods of time.


### Fixed

- Removed the `:after helm` attribute from several imports in
  [init/lit-emacs-init-helm.org]. Those packages should *force* `helm`
  to load instead of waiting for `helm` to load on it's own before
  initialising.

- Removed the `:after pos-tip` attribute from the `flycheck-pos-tip`
  import for the same reason.


- `flycheck-haskell-setup` was hooked on to `flycheck-mode-hook`, it
  should have been `haskell-mode-hook`. This has been fixed.


### Removed

- The comment at the top of [init.el] has been moved to [version
  0](#legacy) below. Versioning/change history is no longer contained
  in the file/files themselves.

- The "Initialisation" heading of [init/lit-emacs-init-general.org]
  has been dropped, along with the "Functions" subheading and the
  [init/lit-emacs-init-load.org] function defined at the start of
  it. I [stopped using](#2018-10-18) that function in a previous
  update.

  The "Additional Files To Load" section has been dropped for the same
  reason.

- Deleted some commented out code in [init/lit-emacs-init-org.org].

- Removed unneccesary `progn` forms in a `use-package` invocation in
  [init/lit-emacs-init-linum.org].

- Removed the per file changelog stuff. This one is easier to
  maintain.





# 0 - Legacy Stuff {#legacy}

Extracted from a simplistic changelog comment I used to keep at the
top of my `init.el` file.

## 2018-10-18

- Added some elisp to compute how long it took to load this file, and
  it's dependencies.  Idea taken from jwiegley's dotemacs Rep.

- Removed some further cruft in ~~this file~~ [init.el], and cleaned
  it up.

- Changed how the imports of lit-init files works. I now manually
  check that whether there are any .elc files, and whether those are
  newer than the corresponding .org files. If yes: load the .elc, if
  no: tangle, compile and load the .org file. This is slow because, in
  addition to tangling and byte-compiling, we also end up having to
  load the org package early so that we can use
  org-tangle-file. Normally, `org` is loaded lazily. The speed up when
  all .elc files are present is noticable, however.


## 2018-10-17

- I've migrated almost all of the contents of this file to a series of
  literate elisp files. I've also switched to a more principled use of
  `use-package` rather than require to reduce load times. This has
  vastly improved startup time, despite the fact that code has to be
  extracted from literate elisp/org files on the fly. Lazy loading
  makes a massive difference.

- Moved the `customize` stuff to a separate file, to prepare for
  splitting this increasingly gargantuan monolith of an initialisation
  file into smaller, more managable parts.

- Deleted an unneccesary `'exec-path` modification that added a path
  that was already avaliable trough the PATH environment variable.

## 2018-10-15

- Got rid of the "H-p" smartparens-strict-mode keybinding, to free up
  that combination for projectile.  Using "s-p" didn't feel quite
  right to me.


## 2018-09-27

- Added an explicit key prefix binding for the `projectile` key map,
  since it no longer exports a default one.

## 2018-06-20

- Added functions for opening the `;;` and `;W` agenda views.  This
  lets me open those by default.


## 2018-06-20

- Removed some code at the end of ~~this file~~ [init.el], having to
  do with `agda-mode`.

## 2017-09-12

- I no longer load `org-bullets` manually.

- Also adds `hlint-refactor-mode`


## 2017-05-29

- Had to get rid of `org-git-link` as it was screwing with
  `org-store-link`.

- Also installed `org-id`

## 2017-05-21

- Upgraded to emacs-25.2, and had to get rid of `rust-flycheck`.

## 2017-05-06

- Adds `SC` again.

- Adds a keybinding for `org-store-link`

## 2017-04-19

- Added `org` as a cask source, so that I could get my hands on the
  `org-contrib` modules. Specifically `org-git-link`.

## 2017-04-15

- Removed my hydralisk thing. I don't use it much anymore.

## 2016-10-07

- Removed global bindings for `org-agenda-list` and `org-todo-list`

- Added binding "H-a" for the more general `org-agenda`.

## 2016-09-13

- Added shortcuts:

  - "H-a" for `org-agenda-list`

  - "H-t" for `org-todo-list`

## 2016-07-13

- Added `auto yas` again.

## 2016-06-08

- Added:

  - `intero-mode`

  - `whitspace-cleanup-mode`

  - `column-enforce-mode`

  - `highlight-indentation-mode`

- Removed:

  - Reliance on `hi2` and `haskell-interactive`

- Updated my hydra defs

## 2016-04-13

- Added my own `win-split` function. Because lazy.

## 2016-02-06

-  Added buf-move

-  Added global key bindings for it, and windmove.

## 2015-12-07

- Removed `SC`. I never use it.

- Added `helm-ag`.

## 2015-11-28

- Removed `helm-swoop` bindings.

  I find `helm-occur` more useful as a search tool.

- Added `encourage-mode`.

- Some minor cleanup.

## 2015-09-09

- Removed the `racer` hacks. This program should work without them
  now, from what I'm told.

## 2015-07-03

- Commented out a line diminishing `magit-auto-revert`.

## 2015-06-08

- Removed `auto-yasnippet` These days I tend to just write temporary
  snippets manually. I tend to make fewer mistakes this way.

## 2015-05-24

-  Swapped `ace` for `avy`

- `helm-swoop` bindings

- Turned off `speedbar`

## 2015-05-24

- Added `ace-jump` stuff:

  - `ace-jump`

  - `ace-isearch`

  - `ace-zap`

- Added a keybinding for `save-buffers-kill-emacs`

## 2015-05-09

- Dropped sublimity.  Was too slow to be useful.

## 2015-04-30

- Added sublimity for minimap

## 2015-04-28

- Added some more minor modes to diminished

## 2015-04-26

- Switched off `nyan` by default

## 2015-04-23

- Added a few minor modes to diminish:

  - `flyspell`

  - `yas`

  - `SP`

  - `magit auto revert`

- Some minor cleanup

- Swapped `mapcar` for `mapc` in a function below since my lint was
  complaining

- Disabeled the `magit` warning

## Updated 2015-04-22

- Added require `projectile` and activated it's use as a global mode

- Added `helm-projectile` mode

- Added `projectile` speedbar mode

- Added more git plugins

- Added `powerline`

## 2015-04-17

- Some minor cleanup

## 2015-02-21

- Added some stuff related to `flyspell`

## 2015-01-27

- Added som `pandoc` stuff.

## 2015-01-14

- Added `helm` + `helm-company` + `helm-hoogle`

- Changed some `company` keybinds

## 2015-01-11

- Having cabal issues. `ghc-mod` is out for now

## 2014-12-19

- Changed several bindings in `haskell-mode` plus `ghc-mod`

- Changed the global `dabbrev-expand` binding

## 2014-12-15

- Added function (stolen from internets) for showing `yasnippets` with
  `popup.el`

## 2014-12-14

- Trying out strict delimter mode as default ... -> Decided I prefer
  regular `smartparens`

- Added keybinding for `dabbrev-expand`

## 2014-12-13

- Added keybindings for window resizing

## 2014-12-10

- Switched off `ghc-mod`; I really only want `ghc-comp-init`

- Some additional clean up

## 2014-12-06

- Added a (pallet-mode t) line.

- Switched from `AC` to `company`

- Installed `ghc-mod`

## 2014-12-05

- Some clean up

## 2014-10-7

- Now relies on `Cask` for most of the package management. Some things
  still need explicit dealing with though.



[init.el]: ./init.el
[init/lit-emacs-init-general.org]: ./init/lit-emacs-init-general.org
[init/lit-emacs-init-linum.org]: ./init/lit-emacs-init-linum.org
[init/lit-emacs-init-helm.org]: ./init/lit-emacs-init-helm.org
[init/lit-emacs-init-templating.org]: ./init/lit-emacs-init-templating.org
