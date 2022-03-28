[![License](https://img.shields.io/github/license/docspell/ds4e.svg?style=flat&color=steelblue)](https://github.com/docspell/ds4e/blob/master/LICENSE.txt)
[![Chat](https://img.shields.io/gitter/room/eikek/docspell?style=flat&color=steelblue&logo=gitter)](https://gitter.im/eikek/docspell)

# Emacs & Docspell

Here are some utilities for using [Docspell](https://docspell.org)
from Emacs! It is a interface around
[dsc](https://github.com/docspell/dsc), please install it as a
prerequisite.

## Installation

### Nix

ds4e uses nix to build. It will inject the current version (from
`version.txt`) into a source file.Run

```
nix-build
```

in the source root or import the file `nix/ds4e.nix` into your setup.

### Manual

Clone this repo and add the `lisp/` directory to emacs' `load-path`.


## Configuration

Use `customize-group ds4e` to edit in Emacs, or set variables
directly. Here are the important ones:

| Variable              | Default              | Description                           |
| :--                   | :--                  | :--                                   |
| `ds4e-dsc-executable` | looked up in `$PATH` | The full path to the `dsc` executable |
| `ds4e-dsc-config`     | -                    | Path to a dsc config file             |
| `ds4e-dsc-server-url` | -                    | Docspell base url                     |
| `ds4e-dsc-verbose`    | 0                    | Verbosity, 0, 1 or 2                  |


No config is required as all settings have sane defaults. Each package
has its own set of configuration options to tweak.

## Contents

### Search view

The `ds4e-search.el` package provides a search feature rendering a
list of items. It is inspired by
[mu4e](https://github.com/djcb/mu/tree/master/mu4e). Start it with
`ds4e-search`.

```emacs-lisp
(use-package ds4e-search
  :load-path "/path/to/ds4e/lisp/"
  :command ds4e-search)
```

### Dired integration

```emacs-lisp
(use-package ds4e-dired
  :load-path "/path/to/ds4e/lisp/"
  :bind (:map dired-mode-map
              ("C-d u" . ds4e-dired-upload)
              ("C-d o" . ds4e-dired-open-browser)))
```

- Upload all marked or the file under point
- Open file under point in docspell (default browser)


### Mu4e integration

There are functions provided that hook into
[mu4e](https://github.com/djcb/mu) (an excellent e-mail client for
Emacs).

```emacs-lisp
(use-package ds4e-mu4e
  :load-path "/path/to/ds4e/lisp/"
  :after (mu4e)
  :config
  (ds4e-mu4e-register))
```

This adds additional commands to the message and header view, for
uploading an attachment or the entire message.


### Dashboard integration

The package `ds4e-dashboard` provides a section for
[dashboard.el](https://github.com/emacs-dashboard/emacs-dashboard). It
can be applied when configuring `dashboard`.

```emacs-lisp
(use-package dashboard
  :config

  ;; load ds4e-dashboard
  (use-package ds4e-dashboard
    :config
    ;; registers the section
    (ds4e-dashboard-register))
  ;; optionally add a shortcut
  (add-to-list 'dashboard-item-shortcuts '(docspell . "d"))

  ;; then configure dashboard as usual, using new section `docspell'
  (setq dashboard-items '((projects . 5)
                          (docspell . 5)
                          (recents  . 5)
                          (agenda . 5)
                          (bookmarks . 5)))
  (setq dashboard-startup-banner 'logo)
  ;; ...
  (dashboard-setup-startup-hook))
```

Note that you need to login first (via `(ds4e-login)`), to allow
querying the database.


### Low-level: run dsc

The package `ds4e-client` has some convenience functions to call
`dsc`. More low-level functions are provided in `ds4e-dsc`.
