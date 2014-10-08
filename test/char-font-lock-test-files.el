;;; char-font-lock-test-files.el --- Regression test for char-font-lock.

;; Copyright (C) 2014 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces languages

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Regression test of `char-font-lock', a package providing font-lock
;; rules for highlighting bad whitespace. This module verifies
;; fontification of a number of files. This is done by keeing a text
;; representation of the fontification using `faceup' markup, in
;; addition to the original files.
;;
;; The actual check is performed using `ert', with font-lock test
;; function provided by `faceup'.

;;; Code:

(require 'faceup)

(defvar char-font-lock-test-dir (faceup-this-file-directory))

(defun char-font-lock-test-file (file)
  "Test that FILE is fontified as the .faceup file describes.

FILE is interpreted as relative to this source directory."
  (faceup-test-font-lock-file 'char-font-lock-mode
                              (concat
                               char-font-lock-test-dir
                               file)))
(faceup-defexplainer char-font-lock-test-file)


(ert-deftest char-font-lock-file-test ()
  (should (char-font-lock-test-file "files/test.txt"))
  (should (char-font-lock-test-file "files/test2.txt")))

;; char-font-lock-test-files.el ends here.
