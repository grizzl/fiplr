;;; fiplr.el --- Fuzzy finder for files in a project.

;; Copyright © 2013 Chris Corbyn
;;
;; Author: Chris Corbyn <chris@w3style.co.uk>
;; URL: https://github.com/d11wtq/fiplr
;; Version: 0.1
;; Keywords: convenience, usability, project

;; This file is not part of GNU Emacs.

;;; --- License

;; Licensed under the same terms as Emacs.

;;; --- Commentary

;; Overview:
;;
;; Fiplr makes it really easy to find files anywhere within your entire
;; project. Files and directories are located by typing just a few characters
;; in the order in which they appear in the path, but not necessarily adjacent
;; to one another.
;;
;; For example, if you have a file ./lib/game/leaderboards.rb, you can
;; just run `M-x fiplr-find-file` and type `gldr` (G-ame L-ea-D-e-R-boards)
;; and your file will be listed. Navigate between multiple matches with the
;; left and right arrow keys and select one with the return key.
;;
;; You may add multiple patterns that are order-independent of one another
;; by using the ";" character to separate the patterns. Suppose we had typed
;; just `ldr` in the above example and too many matches come up. Rather than
;; backspace to add part of the directory name and narrow the search down,
;; we can just type `;ga`. Read this like an "and" operator.
;;
;; Usage:
;;
;; Run `M-x fiplr-find-file` to find files.
;; Run `M-x fiplr-find-directory` to find directories.
;; Run `M-x fiplr-find-buffer` to find open buffers.
;; Run `M-x fiplr-clear-cache` to empty the file cache.
;;
;; Once fiplr is running:
;;
;; Switch between modes with `C-f`, `C-d` and `C-b`.
;; Reset and reload with `C-r`.
;; 
;; For convenience, bind `C-p` to `fiplr-find-file`:
;;
;;   (global-set-key (kbd "C-p") 'fiplr-find-file)
;;

;;; --- Package Configuration

;; The default set of files/directories to look for at the root of a project.
(defvar *fiplr-default-root-markers*
  '(".git" ".svn" ".hg" ".bzr"))

;; The default set of patterns to exclude from searches.
(defvar *fiplr-default-ignored-globs*
  '((directories (".git" ".svn" ".hg" ".bzr"))
    (files (".#*" "*.so"))))

;; Customization group declaration.
(defgroup fiplr nil
  "Configuration options for fiplr - find in project.")

;; Settings for project root directories.
(defcustom fiplr-root-markers *fiplr-default-root-markers*
  "A list of files or directories that are found at the root of a project."
  :type    '(repeat string)
  :group   'fiplr
  :options *fiplr-default-root-markers*)

;; Settings for files and directories that should be ignored.
(defcustom fiplr-ignored-globs *fiplr-default-ignored-globs*
  "An alist of glob patterns to exclude from search results."
  :type    '(alist :key-type symbol :value-type (repeat string))
  :group   'fiplr
  :options *fiplr-default-ignored-globs*)

;;; --- Public Functions

;; Defines fiplr's determination of the project root.
(defun fiplr-root ()
  "Locate the root of the project by walking up the directory tree."
  "The first directory containing one of fiplr-root-markers is the root."
  "If no root marker is found, the current working directory is used."
  (let ((cwd (if (buffer-file-name)
                 (directory-file-name
                  (file-name-directory (buffer-file-name)))
               (expand-file-name "."))))
    (or (fiplr-find-root cwd fiplr-root-markers)
        cwd)))

;;; --- Private Functions

;; Search algorithm to find dir with .git etc.
(defun fiplr-find-root (path root-markers)
  "Tail-recursive part of project-root."
  (let* ((this-dir (file-name-as-directory path))
         (parent-dir (expand-file-name (concat this-dir "..")))
         (system-root-dir (expand-file-name "/")))
    (cond
     ((fiplr-root-p path root-markers) this-dir)
     ((equal system-root-dir this-dir) nil)
     (t (fiplr-find-root parent-dir root-markers)))))

;; Predicate looking at path for a root marker.
(defun fiplr-root-p (path root-markers)
  "Predicate to check if the given directory is a project root."
  (let ((dir (file-name-as-directory path)))
    (cl-member-if (lambda (marker)
                    (file-exists-p (concat dir marker)))
                  root-markers)))

;; Builds a gigantic `find' shell command with -prune, -o, -not and shit.
(defun fiplr-list-files-shell-command (type path ignored-globs)
  "Builds the `find' command to locate all project files & directories."
  "Path is the base directory to recurse from."
  "Ignored-globs is an alist with keys 'directories and 'files."
  (cl-labels ((type-abbrev (assoc-type)
                (cl-case assoc-type
                  ('directories "d")
                  ('files "f")))
              (name-matcher (glob)
                (mapconcat 'identity
                           `("-name" ,(shell-quote-argument glob))
                           " "))
              (grouped-name-matchers (type)
                (mapconcat 'identity
                           `(,(shell-quote-argument "(")
                             ,(mapconcat #'name-matcher
                                      (cadr (assoc type ignored-globs))
                                      " -o ")
                             ,(shell-quote-argument ")"))
                           " "))
              (matcher (assoc-type)
                (mapconcat 'identity
                           `(,(shell-quote-argument "(")
                             "-type"
                             ,(type-abbrev assoc-type)
                             ,(grouped-name-matchers assoc-type)
                             ,(shell-quote-argument ")"))
                           " ")))
    (mapconcat 'identity
               `("find"
                 ,(shell-quote-argument (directory-file-name path))
                 ,(matcher 'directories)
                 "-prune"
                 "-o"
                 "-not"
                 ,(matcher 'files)
                 "-type"
                 ,(type-abbrev type)
                 "-print")
               " ")))

;; List all files found under the given path, ignoring ignored-globs.
(defun fiplr-list-files (type path ignored-globs)
  "Expands to a flat list of files/directories found under path."
  "The first parameter - type - is the symbol 'directories or 'files."
  (let ((list-string
         (shell-command-to-string (fiplr-list-files-shell-command
                                   type
                                   path
                                   ignored-globs))))
    (split-string list-string "[\r\n]+" t)))

;; Create a fuzzy search index for the given set of strings.
(defun fiplr-make-index (strings)
  "Makes a fast lookup table from strings for use with `fiplr-index-lookup'."
  "The index assumes that the input list ordering will not change."
  (let ((hash-table (make-hash-table)))
    (cl-reduce (lambda (list-offset str)
                 (fiplr-index-insert-string str list-offset hash-table)
                 (1+ list-offset))
               strings
               :initial-value 0)
    (maphash (lambda (char str-map)
               (maphash (lambda (list-offset locations)
                          (puthash list-offset (reverse locations) str-map))
                        str-map)) hash-table)
    hash-table))

;; Insert the string at list-offset into the index.
(defun fiplr-index-insert-string (string list-offset hash-table)
  "This is an internal function used by `fiplr-make-index'."
  "It inserts the string stored at list-offset of the main list."
  "An explanation of the data structure and algorithm can be found at http://link.com/."
  (cl-reduce (lambda (char-offset char)
               (let* ((str-map (or (gethash char hash-table)
                                   (puthash char (make-hash-table) hash-table)))
                      (offsets (gethash list-offset str-map)))
                 (puthash list-offset
                          (cons char-offset offsets)
                          str-map)
                 (1+ char-offset)))
             string
             :initial-value 0))

;; Perform a fuzzy-search in the index and return a result hash.
(defun fiplr-index-search (term index)
  "Fuzzy searches for term in an index prepared with `fiplr-make-index'."
  "The result format is unspecified and can be read with `fiplr-read-result'."
  "Characters must appear in the same order, but need not be adjacent."
  (let ((result (make-hash-table)))
    (cl-reduce (lambda (pos char)
                 (let ((str-map (gethash char index)))
                   (if (> pos 0)
                       (maphash (lambda (str-offset char-offset)
                                  (let* ((char-offsets (gethash str-offset str-map))
                                         (new-offset (cl-member-if (lambda (x)
                                                                     (> x char-offset))
                                                                char-offsets)))
                                    (if new-offset
                                        (puthash str-offset (car new-offset) result)
                                      (remhash str-offset result))
                                    result))
                                result)
                     (maphash (lambda (str-offset char-offsets)
                                (puthash str-offset (car char-offsets) result))
                              str-map))
                   (1+ pos)))
               term
               :initial-value 0)
    result))

;; Extract the elements of the list that are present in the search result.
(defun fiplr-read-result (search-result strings)
  "Reads the elements from the list strings, which are in the search result."
  ; FIXME: This iterates over all strings, instead of just the result keys
  (cdr (cl-reduce (lambda (idx-and-matches str)
                    (let ((idx (car idx-and-matches))
                          (matches (cdr idx-and-matches)))
                      (if (gethash idx search-result)
                          (cons (1+ idx) (cons str matches))
                        (cons (1+ idx) matches)))) strings :initial-value (cons 0 nil))))

;;; --- Package Export

(provide 'fiplr)
