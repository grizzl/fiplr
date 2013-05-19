# Fiplr - Find in Project for Emacs

Fiplr (pronounced FIP-ler, as in _Find in Project_) is an Emacs package to
allow you to locate and open files deep within a complex directory tree,
using fuzzy matching.

A key design goal is to make Fiplr really easy to use with little-to-no
configuration, beyond a single key binding.

It is heavily inspired by Vim's [ctrlp](https://github.com/kien/ctrlp.vim) and
TextMate's Command-T.

Fiplr currently requires ido, though you need not have `ido-mode` turned on.
This means it will work in Emacs 24 or later, but in older versions you should
make sure you have ido installed and loaded.

## Installation:

The easiest way to install Fiplr is through `package.el` +
[Marmalade](http://marmalade-repo.org/) or [MELPA](http://melpa.milkbox.net/):

    M-x package-install RET fiplr RET

If you're not using `package.el`, just put fiplr.el on your load path and require it.

    (add-to-list 'load-path "~/.emacs.d/fiplr")
    (require 'fiplr)

## Usage:

Currently it uses ido, because that was quick to get up and running with.

    M-x fiplr-find-file

By default it looks through all the parent directories of the file you're
editing until it finds a .git, .hg, .bzr or .svn directory. You can
customize this list of root markers by setting `fiplr-root-markers`.

    (setq fiplr-root-markers '(".git" ".svn"))

Some files are ignored from the directory tree because they are not text
files, or simply to speed up the search. The default list can be
customized by setting `fiplr-ignored-globs`.

    (setq fiplr-ignored-globs '((directories (".git" ".svn"))
                                (files ("*.jpg" "*.png" "*.zip" "*~"))))

These globs are used by the UNIX `find' command's -name flag.

Commands:

  * Find files:        <kbd>M-x</kbd> fiplr-find-file <kbd>RET</kbd>
  * Find directories:  <kbd>M-x</kbd> fiplr-find-directory <kbd>RET</kbd>
  * Clear caches:      <kbd>M-x</kbd> fiplr-clear-cache <kbd>RET</kbd>

Fiplr caches the directory tree to avoid rescanning every time it is run. You
should use `fiplr-clear-cache` as shown above if you have added files etc.

For convenience, bind <kbd>C-x</kbd><kbd>f</kbd> to `fiplr-find-file`:

    (global-set-key (kbd "C-x f") 'fiplr-find-file)

## Future Plans

This version that is using ido mode is just a temporary solution. Ido doesn't
really handle huge search lists as well as it could do, so there is work
underway on the "no-ido" branch to perform the fuzzy searching using an index
designed specifically for this problem (it uses a three-dimensional lookup
table that is rapidly reduced as more characters are entered at the prompt).

The user interface will remain the same, however, so this is simply an
optimization.

Secondly, I'd love to add `fiplr-apropos` which fuzzy searches all Elisp
symbols and shows the documentation in real time, and `fiplr-find-command`
which does the same thing for interactive command documentation.

## Copyright & Licensing

Copyright (c) Chris Corbyn 2013, Licensed under the same terms as GNU Emacs.
