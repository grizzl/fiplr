;;; fiplr.el --- Fuzzy finder for files in a project.

;; Copyright © 2013 Chris Corbyn

;;; --- Unit Tests

(load (expand-file-name "../fiplr-search-index.el"))

(require 'fiplr-search-index)

(ert-deftest index-search-test ()
  "Test fiplr can create and fuzzy-search an index of strings."
  (let* ((strings '("models" "controllers" "views"))
         (index (fiplr-make-index strings)))
    (cl-flet ((search (term)
                (let ((result (fiplr-index-search term index nil)))
                  (fiplr-read-result result index))))
      (should (equal (search "oe")  '("controllers" "models")))
      (should (equal (search "iw")  '("views")))
      (should (equal (search "bad") '()))
      (should (equal (search "es")  '("views" "controllers" "models"))))))

(ert-deftest continue-index-search-test ()
  "Test fiplr can accept an existing result and search string to search."
  (let* ((strings '("models" "controllers" "views"))
         (index (fiplr-make-index strings))
         (result (fiplr-index-search "ol" index nil))
         (matches (fiplr-read-result (fiplr-index-search "olr" index result)
                                     index)))
    (should (equal matches  '("controllers")))))

(ert-deftest continue-index-search-backspace-test ()
  "Test fiplr handles repeated search when backspace was hit."
  (let* ((strings '("models" "controllers" "views"))
         (index (fiplr-make-index strings))
         (result (fiplr-index-search "olr" index nil))
         (matches (fiplr-read-result (fiplr-index-search "ol" index result)
                                     index)))
    (should (equal matches  '("controllers" "models")))))

;; TODO: Implement and test Levenshtein ordering
