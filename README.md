# Fiplr - Find in Project for Emacs

Fiplr (pronounced FIP-ler, as in _Find in Project_) is an Emacs package to
allow you to locate and open files deep within a complex directory tree,
using fuzzy matching.

![Screenshot](http://i.imgur.com/n3EweV3.png)

A key design goal is to make Fiplr really easy to use with little-to-no
configuration, beyond a single key binding.

It is heavily inspired by Vim's [ctrlp](https://github.com/kien/ctrlp.vim), TextMate's `Command-T`, and Sublime's `Control+P`.

Internally it uses [Grizzl](https://github.com/d11wtq/grizzl) to do the fuzzy
searching.

## Installation:

The easiest way to install Fiplr is through `package.el` +
[MELPA](http://melpa.milkbox.net/):

    M-x package-install RET fiplr RET

## Usage:

Run `fiplr-find-file` and the minibuffer will open, along with a menu of files
in your project. Start typing and use the arrow keys to pick a file.

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

  * Find files:       <kbd>M-x</kbd> fiplr-find-file <kbd>RET</kbd>
  * Find directories: <kbd>M-x</kbd> fiplr-find-directory <kbd>RET</kbd>
  * Clear caches:     <kbd>M-x</kbd> fiplr-clear-cache <kbd>RET</kbd>

Fiplr caches the directory tree to avoid rescanning every time it is run. You
can reload the file list during a search by hitting <kbd>C-c r</kbd> while
fiplr is running. You may alternatively use `fiplr-clear-cache`.

For convenience, bind <kbd>C-x f</kbd> to `fiplr-find-file`:

    (global-set-key (kbd "C-x f") 'fiplr-find-file)

## Copyright & Licensing

Copyright (c) Chris Corbyn 2013, Licensed under the same terms as GNU Emacs.
