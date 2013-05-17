;;; fiplr.el --- Fuzzy finder for files in a project.

;; Copyright © 2013 Chris Corbyn

;;; --- Unit Tests

(load (expand-file-name "../fiplr.el"))

(require 'fiplr)

(ert-deftest find-git-root-test ()
  "Test that fiplr can find the root of a project."
  (should (equal (expand-file-name "./fixture/")
                 (fiplr-find-root "./fixture/ext/sample" '(".bzr")))))

(ert-deftest find-non-root-test ()
  "Test that fiplr returns nil if there is no root marker"
  (should (equal (fiplr-find-root "./fixture/ext/sample" '(".foo"))
                 nil)))

(ert-deftest list-files-test ()
  "Test that fiplr is able to list all files in a project."
  (should (equal (fiplr-list-files 'files "./fixture"
                                   '((files (".#*"))
                                     (directories (".bzr"))))
                 '("./fixture/ext/sample/sample.c"
                   "./fixture/lib/sample/version.rb"
                   "./fixture/lib/sample.rb"
                   "./fixture/README.md"
                   "./fixture/sample.gemspec"
                   "./fixture/spec/sample_spec.rb"
                   "./fixture/spec/spec_helper.rb"))))

(ert-deftest list-directories-test ()
  "Test that fiplr is able to list all directories in a project."
  (should (equal (fiplr-list-files 'directories "./fixture"
                                   '((files (".#*"))
                                     (directories (".bzr"))))
                 '("./fixture"
                   "./fixture/ext"
                   "./fixture/ext/sample"
                   "./fixture/lib"
                   "./fixture/lib/sample"
                   "./fixture/spec"))))

(ert-deftest index-search-test ()
  "Test fiplr can create and fuzzy-search an index of strings."
  (let* ((strings '("models" "controllers" "views"))
         (index (fiplr-make-index strings)))
    (cl-flet ((search (term)
               (let ((result (fiplr-index-search term index)))
                 (fiplr-read-result result index))))
      (should (equal (search "oe")  '("controllers" "models")))
      (should (equal (search "iw")  '("views")))
      (should (equal (search "bad") '()))
      (should (equal (search "es")  '("views" "controllers" "models"))))))
