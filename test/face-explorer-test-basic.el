;;; face-explorer-test-basic.el --- Basic tests for `face-explorer'.

;; Copyright (C) 2017 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces

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

;; Test primitive function in `face-explorer'.

;;; Code:

(require 'face-explorer)

(ert-deftest face-explorer-test-basic-plist-set ()
  (should (equal (face-explorer-plist-set '() :foo 10)
                 '(:foo 10)))
  (should (equal (face-explorer-plist-set '(:foo 5) :foo 10)
                 '(:foo 10)))
  (should (equal (face-explorer-plist-set '(:bar 5) :foo 10)
                 '(:foo 10 :bar 5)))
  (should (equal (face-explorer-plist-set '(:alpha 1 :foo 5) :foo 10)
                 '(:alpha 1 :foo 10)))
  (should (equal (face-explorer-plist-set '(:alpha 1 :foo 5) :foo 10)
                 '(:alpha 1 :foo 10)))
  (should (equal (face-explorer-plist-set '(:foo 5 :beta 2) :foo 10)
                 '(:foo 10 :beta 2)))
  (should (equal (face-explorer-plist-set '(:alpha 1 :foo 5 :beta 2) :foo 10)
                 '(:alpha 1 :foo 10 :beta 2)))
  )

(ert-deftest face-explorer-test-basic-fix-face-attributes ()
  "Test `face-explorer-fix-face-attributes'."
  (should (equal (face-explorer-fix-face-attributes '())
                 '()))
  (should (equal (face-explorer-fix-face-attributes '(:foo 10))
                 '(:foo 10)))
  (should (equal (face-explorer-fix-face-attributes '(:bold t :foo 10))
                 '(:weight bold :foo 10)))
  (should (equal (face-explorer-fix-face-attributes '(:bold nil :foo 10))
                 '(:weight normal :foo 10)))
  (should (equal (face-explorer-fix-face-attributes '(:b 20 :bold t :foo 10))
                 '(:b 20 :weight bold :foo 10)))
  (should (equal (face-explorer-fix-face-attributes '(:b 20 :bold nil :foo 10))
                 '(:b 20 :weight normal :foo 10)))
  ;; Verify that a new list only is constructed when needed.
  (let ((var '(:foo 10)))
    (should (eq (face-explorer-fix-face-attributes var)
                var))
    (should (equal var '(:foo 10)))))

(ert-deftest face-explorer-test-remove-plist-duplicates ()
  (should (equal (face-explorer-remove-plist-duplicates '()) '()))
  (should (equal (face-explorer-remove-plist-duplicates '(:a 1)) '(:a 1)))
  (should (equal (face-explorer-remove-plist-duplicates
                  '(:a 1 :b 2)) '(:a 1 :b 2)))
  (should (equal (face-explorer-remove-plist-duplicates
                  '(:a 1 :a 2)) '(:a 2))))

(ert-deftest face-explorer-test-plist ()
  (should (equal (face-explorer-plist-delete :foo '())
                 '()))
  (should (equal (face-explorer-plist-delete :foo '(:foo 10))
                 '()))
  (should (equal (face-explorer-plist-delete :foo '(:bar 10))
                 '(:bar 10)))
  (should (equal (face-explorer-plist-delete :other '(:foo 10 :bar 10 :baz 10))
                 '(:foo 10 :bar 10 :baz 10)))
  (should (equal (face-explorer-plist-delete :baz '(:foo 10 :bar 10 :baz 10))
                 '(:foo 10 :bar 10)))
  (should (equal (face-explorer-plist-delete :bar '(:foo 10 :bar 10 :baz 10))
                 '(:foo 10 :baz 10)))
  (should (equal (face-explorer-plist-delete :foo '(:foo 10 :bar 10 :baz 10))
                 '(:bar 10 :baz 10))))


(ert-deftest face-explorer-maybe-set-flag ()
  (should (equal (face-explorer-maybe-set-inherit-from-default-flag
                  '(nil . (:foo t))
                  nil)
                  '(nil . (:foo t))))
  (should (equal (face-explorer-maybe-set-inherit-from-default-flag
                  '(nil . (:foo t))
                  t)
                  '(t . (:foo t))))
  (should (equal (face-explorer-maybe-set-inherit-from-default-flag
                  '(t . (:foo t))
                  nil)
                  '(t . (:foo t))))
  (should (equal (face-explorer-maybe-set-inherit-from-default-flag
                  '(t . (:foo t))
                  t)
                  '(t . (:foo t)))))

(ert-deftest face-explorer-format-tooltip ()
  (let ((plist '(face x fontified t)))
    (should (equal (face-explorer-tooltip-format-plist '(face) plist)
                   '("face: x")))
    (should (equal (face-explorer-tooltip-format-plist '(not) plist)
                   '("fontified: t" "face: x")))
    (should (equal (face-explorer-tooltip-format-plist '(not fontified) plist)
                   '("face: x")))))

(ert-deftest face-explorer-table-shift-cells ()
  (let ((lines '((alpha beta gamma delta epsilon)
                 (0 1 2 3 4))))
    (should (equal (face-explorer-table-shift-cells (copy-tree lines) 1 3 0)
                   lines))
    (should (equal (face-explorer-table-shift-cells (copy-tree lines) 1 3 1)
                   '((alpha delta beta gamma epsilon)
                     (0 3 1 2 4))))
    (should (equal (face-explorer-table-shift-cells (copy-tree lines) 1 3 2)
                   '((alpha gamma delta beta epsilon)
                     (0 2 3 1 4))))
    (should (equal (face-explorer-table-shift-cells (copy-tree lines) 1 3 3)
                   lines))
    nil))

;; ------------------------------------------------------------
;; Display matcher.
;;


(ert-deftest face-explorer-match-display-requirement ()
  ""
  (dolist (entry '((nil :no-match)
                   (8   :no-match)
                   (16  (:foreground "red"))
                   (t   (:foreground "red"))))
    (let ((face-explorer-number-of-colors (nth 0 entry)))
      (should (equal (face-explorer-match-display-requirements
                      '((((min-colors 16)) :foreground "red"))
                      :no-match)
                     (nth 1 entry))))))


;; ------------------------------------------------------------
;; Table generator.
;;

(ert-deftest face-explorer-table ()
  "Test `face-explorer-insert-table'."
  (let ((lines '()))
    (push '("Alpha" "Beta") lines)
    (push '(:table-line :table-line) lines)
    (push '("One" "Two") lines)
    (push '((:table-callback
             (lambda ()
               nil))
            "Red"
            (:table-callback
             (lambda ()
               nil))
            "Green"
            (:table-callback
             (lambda ()
               nil)))
          lines)
    ;; Cell:s returning the content.
    (push '((:table-cell
             (lambda (width)
               "Yellow"))
            (:table-cell
             (lambda (width)
               "Blue")))
          lines)
    ;; Cell:s inserting.
    (push '((:table-cell
             (lambda (width)
               (when width
                 (insert "X"))
               1))
            (:table-cell
             (lambda (width)
               (when width
                 (insert "Y"))
               1)))
          lines)
    (should (equal
             (with-temp-buffer
               (face-explorer-insert-table (reverse lines))
               (buffer-string))
             "\
Alpha  Beta
------ -----
One    Two
Red    Green
Yellow Blue
X      Y
"))))


;; ------------------------------------------------------------
;; The end
;;

(provide 'face-explorer-test-basic)

;;; face-explorer-test-basic.el ends here
