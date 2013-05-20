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
  (let ((files (sort (fiplr-list-files
                      'files
                      "./fixture"
                      '((files (".#*"))
                        (directories (".bzr")))) #'string-lessp)))
        (should (equal files
                       '("README.md"
                         "ext/sample/sample.c"
                         "lib/sample.rb"
                         "lib/sample/version.rb"
                         "sample.gemspec"
                         "spec/sample_spec.rb"
                         "spec/spec_helper.rb")))))

(ert-deftest list-directories-test ()
  "Test that fiplr is able to list all directories in a project."
  (should (equal (fiplr-list-files 'directories "./fixture"
                                   '((files (".#*"))
                                     (directories (".bzr"))))
                 '("ext"
                   "ext/sample"
                   "lib"
                   "lib/sample"
                   "spec"))))

(ert-deftest list-files-symlinked-test ()
  "Test that fiplr is able to understand projects at symlinked paths."
  (let ((files (sort (fiplr-list-files
                      'files
                      "./fixture-symlink"
                      '((files (".#*"))
                        (directories (".bzr")))) #'string-lessp)))
        (should (equal files
                       '("README.md"
                         "ext/sample/sample.c"
                         "lib/sample.rb"
                         "lib/sample/version.rb"
                         "sample.gemspec"
                         "spec/sample_spec.rb"
                         "spec/spec_helper.rb")))))
