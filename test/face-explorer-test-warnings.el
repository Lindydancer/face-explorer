;;; face-explorer-test-basic.el --- Test warnings in `face-explorer'.  -*- lexical-binding: t; -*-

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

;; Test for the face warning system of `face-explorer'.

;;; Code:

(require 'face-explorer)

(defface face-explorer-test-dont-inherit-from-default
  '((t))
  "Face that doesn't inherit from the `default' face.")

(defface face-explorer-test-inherit-from-default1
  '((t :inherit default))
  "Face that directly inherit from the `default' face.")

(defface face-explorer-test-inherit-from-default2
  '((t :inherit face-explorer-test-inherit-from-default1))
  "Face that indirectly inherit from the `default' face.")


;; ------------------------------------------------------------
;; Tests
;;

(ert-deftest face-explorer-test-warnings-inherit-from-default ()
  (should (not (eq (face-explorer-verify-dont-inherit-from-default
                    'face-explorer-test-inherit-from-default1)
                   '())))
  (should (not (eq (face-explorer-verify-dont-inherit-from-default
                    'face-explorer-test-inherit-from-default2)
                   '())))
  (should (eq (face-explorer-verify-dont-inherit-from-default
               'face-explorer-test-dont-inherit-from-default)
              '()))
  (should (eq (face-explorer-verify-dont-inherit-from-default
               'default)
              '()))
  nil)


;; ------------------------------------------------------------
;; The end
;;

(provide 'face-explorer-test-warnings)

;;; face-explorer-test-warnings.el ends here
