;;; fiplr-search-index.el --- Fuzzy search index based on Levenshtein distance.

;; Copyright © 2013 Chris Corbyn
;;
;; Author: Chris Corbyn <chris@w3style.co.uk>
;; URL: https://github.com/d11wtq/fiplr
;; Version: 0.1.2
;; Keywords: convenience, usability, project

;; This file is not part of GNU Emacs.

;;; --- License

;; Licensed under the same terms as Emacs.

;;; --- Commentary

;; Overview:
;;
;; The functions provided by this package provide the search algorithm used by
;; fiplr.el. The algorithm is based on a shallow lookup table that maps
;; characters in the search string to matching files/directories in the dataset
;; along with the locations at which those characters occur. This allows
;; searches to be performed in such a way that they actually become more
;; efficient, the more characters are typed in the search term (since the
;; matching entries from the index are reduced with each additional character).
;; Additionally, because the offsets are stored, the Levenshtein distance
;; between the search term and the matches can be tracked with no additional
;; computation. This allows for the most appropriate matches to be returned
;; first.
;;

(require 'cl)

;;; --- Public Functions

;; Create a fuzzy search index for the given set of strings.
;;;###autoload
(defun fiplr-make-index (strings)
  "Makes a fast lookup table from strings for use with `fiplr-index-lookup'."
  "An explanation of the data structure and algorithm can be found at:"
  "https://github.com/d11wtq/fiplr/issues/4"
  (let ((hash-table (make-hash-table)))
    (reduce (lambda (list-offset str)
              (fiplr-lookup-table-insert-string str list-offset hash-table)
              (1+ list-offset))
            strings
            :initial-value 0)
    (maphash (lambda (char str-map)
               (maphash (lambda (list-offset locations)
                          (puthash list-offset (reverse locations) str-map))
                        str-map)) hash-table)
    (cons (vconcat strings) hash-table)))

;; Prompt th user to choose an item from the index.
;;;###autoload
(defun fiplr-completing-read (prompt index)
  "Use INDEX to offer fuzzy completions to the user as they type at the prompt."
  (let ((overlay nil)
        (index index)
        (result (fiplr-reset-result "" nil)))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq overlay (make-overlay (point-min) (point-min)))
          (add-hook 'post-command-hook
                    'fiplr-minibuffer-post-command-hook nil t)
          (add-hook 'minibuffer-exit-hook
                    'fiplr-minibuffer-exit-hook nil t))
      (read-from-minibuffer prompt))))

;; Perform a fuzzy-search in the index and return a result hash.
;;;###autoload
(defun fiplr-index-search (term index old-result)
  "Fuzzy searches for term in an index prepared with `fiplr-make-index'."
  "The result format is unspecified and can be read with `fiplr-read-result'."
  "Characters must appear in the same order, but need not be adjacent."
  (let* ((result (fiplr-reset-result term old-result))
         (matches (fiplr-result-matches result))
         (from-pos (length (fiplr-result-search-term result)))
         (remainder (substring term from-pos))
         (table (fiplr-get-lookup-table index)))
    (reduce (lambda (n ch)
              (let ((sub-table (gethash ch table)))
                (if (not sub-table)
                    (clrhash matches)
                  (if (> n 0)
                      (fiplr-index-search-continue sub-table matches)
                    (fiplr-index-search-init sub-table matches)))
                (1+ n)))
            remainder
            :initial-value from-pos)
    (fiplr-make-result term matches)))

;; Extract the elements of the list that are present in the search result.
;;;###autoload
(defun fiplr-read-result (result index)
  "Reads the elements from the list strings, which are in the search result."
  "Results are sorted by Levenshtein distance."
  (let* ((matches (fiplr-result-matches result))
         (strings (fiplr-get-strings index))
         (loaded '()))
    (maphash (lambda (string-offset _)
               (push (elt strings string-offset) loaded))
             matches)
    loaded))

;;; --- Private Functions

;; Handle interaction with the minibuffer during a completing read.
(defun fiplr-minibuffer-post-command-hook ()
  "Internal function used by fiplr-completing-read."
  (unless (string-equal (minibuffer-contents)
                        (fiplr-result-search-term result))
    (setq result (fiplr-index-search (minibuffer-contents)
                                     index
                                     result))
    (fiplr-display-matches result index overlay)))

;; Exit hook for fiplr-completing-read.
(defun fiplr-minibuffer-exit-hook ()
  "Internal function used by fiplr-completing-read."
  (delete-overlay overlay)
  (remove-hook 'post-command-hook
               'fiplr-minibuffer-post-command-hook t)
  (remove-hook 'minibuffer-exit-hook
               'flipr-minibuffer-exit-hook t))

;; Show the results of a search in the overlay view in the minibuffer.
(defun fiplr-display-matches (result index overlay)
  "Internal function to display search results in the minibuffer."
  (let* ((matches (fiplr-read-result result index))
         (page (delete-if-not 'identity (subseq matches 0 10))))
    (overlay-put overlay
                 'before-string
                 (format "%s\nTotal Found: %d\n"
                         (mapconcat 'identity page "\n")
                         (length matches)))
    (set-window-text-height nil (+ 2 (length page)))))

;; Prepare a blank search result.
(defun fiplr-make-result (term matches)
  "Creates a new result data structure used in fuzzy-searching."
  "The nature of the structure is subject to change."
    (cons term matches))

;; Check if the given result matches the search term and give a blank one if not.
(defun fiplr-reset-result (term result)
  "Inspects the current search term against an existing result."
  "Either returns the existing result, or a blank result if needed."
  (if result
      (let* ((old-term (fiplr-result-search-term result))
             (new-len (length term))
             (old-len (length old-term)))
        (if (and (> new-len old-len)
                 (string-equal old-term (substring term 0 old-len)))
              result
          (fiplr-make-result "" (make-hash-table))))
    (fiplr-make-result "" (make-hash-table))))

;; Read the search term used from the result.
(defun fiplr-result-search-term (result)
  "Returns the search term used to find the matches in the result."
  (car result))

;; Read the internal matches hash-table from the result.
(defun fiplr-result-matches (result)
  "Returns a hash containing char offsets & Levenshtein distances for each match."
  (cdr result))

;; Insert the string at list-offset into the index.
(defun fiplr-lookup-table-insert-string (string list-offset index)
  "This is an internal function used by `fiplr-make-index'."
  "It inserts the string stored at list-offset of the main list."
  (reduce (lambda (char-offset char)
            (let* ((str-map (or (gethash char index)
                                (puthash char (make-hash-table) index)))
                   (offsets (gethash list-offset str-map)))
              (puthash list-offset
                       (cons char-offset offsets)
                       str-map)
              (1+ char-offset)))
          string
          :initial-value 0))

;; Get the lookup table portion of the fuzzy search index.
(defun fiplr-get-lookup-table (index)
  "Returns the lookup table used for fuzzy-searching."
  "The internal structure of the index is left undefined and subject to change."
  (cdr index))

;; Get the string data set from the fuzzy search index.
(defun fiplr-get-strings (index)
  "Returns the vector of strings used in the index construction."
  "The internal structure of the index is left undefined and subject to change."
  (car index))

;; Populates the results based on the first character matched.
(defun fiplr-index-search-init (sub-table result)
  "Used to initialize potential matches if the first char matched in search."
  (maphash (lambda (k v)
             (puthash k (car v) result))
           sub-table))

;; Filters down already-matching results.
(defun fiplr-index-search-continue (sub-table result)
  "Use the search lookup table to filter already-accumulated results."
  (cl-flet ((next-offset (key current sub-table)
              (find-if (lambda (v)
                         (> v current))
                       (gethash key sub-table))))
    (maphash (lambda (k v)
               (let ((offset (next-offset k v sub-table)))
                 (if offset
                     (puthash k offset result)
                   (remhash k result))))
             result)))

(provide 'fiplr-search-index)

;;; fiplr-search-index.el ends here
