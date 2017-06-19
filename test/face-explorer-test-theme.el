;;; face-explorer-test-theme.el --- test for face-explorer

;; Copyright (C) 2017  Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
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

;; Theme-related tests for `face-explorer'.

;;; Code:

(require 'face-explorer)

(deftheme face-explorer-test-theme
  "Theme to test `face-explorer'.")


(defface face-explorer-test1
  '((t :foreground "red"))
  "Face for testing `face-explorer'.")

(defface face-explorer-test2
  '((t :foreground "red"))
  "Face for testing `face-explorer'.")

(defface face-explorer-test3
  '((t :foreground "red"))
  "Face for testing `face-explorer'.")

(defface face-explorer-test-type
  '((((type tty)) :foreground "red")
    (((type graphic)) :foreground "green")
    (t :foreground "blue"))
  "Face for testing `face-explorer'.")


;; Note: The display requirement list is empty, to make sure this
;; works in batch mode, where the display doesn't support anything.
(let ((req '()))
  (custom-theme-set-faces
   'face-explorer-test-theme
   ;; Recommended (DISPLAY . ATTS)
   `(face-explorer-test1 ((,req :foreground "green")))
   ;; Deprecated (DISPLAY ATTS)
   `(face-explorer-test2 ((,req (:foreground "green"))))
   ;; Malformed.
   ;;
   ;; Note: The ambition is not to be 100% compatible for malformed
   ;; entities, but it doesn't hurt to ensure that the ones stumbled
   ;; on behave correctly.
   `(face-explorer-test3
     ((,req (:foreground "green") (:background "red"))))))

(provide-theme 'face-explorer-test-theme)

(ert-deftest face-explorer-test-theme ()
  "Test theme face definitions."
  (face-explorer-with-fictitious-display-as-frame (selected-frame)
    (should
     (face-explorer-test-faces-plist-equal
      (face-explorer-face-attributes
       'face-explorer-test1)
      '(:foreground "green")))
    (should
     (face-explorer-test-faces-plist-equal
      (face-explorer-face-attributes-for-fictitious-display
       'face-explorer-test1)
      '(:foreground "green")))
    (should
     (face-explorer-test-faces-plist-equal
      (face-explorer-face-attributes
       'face-explorer-test2)
      '(:foreground "green")))
    (should
     (face-explorer-test-faces-plist-equal
      (face-explorer-face-attributes-for-fictitious-display
       'face-explorer-test2)
      '(:foreground "green")))
    (should
     (face-explorer-test-faces-plist-equal
      (face-explorer-face-attributes
       'face-explorer-test3)
      '()))
    (should
     (face-explorer-test-faces-plist-equal
      (face-explorer-face-attributes-for-fictitious-display
       'face-explorer-test3)
      '()))))


;; -------------------------------------------------------------------
;; Face defined in multiple themes.
;;

(defface face-explorer-test-theme-face1 '((t)) "")
(defface face-explorer-test-theme-face2 '((t)) "")

(deftheme face-explorer-test-theme1 "")

(custom-theme-set-faces
 'face-explorer-test-theme1
 '(face-explorer-test-theme-face1 ((t :foreground "red")))
 '(face-explorer-test-theme-face2 ((t :underline t))))

(provide-theme 'face-explorer-test-theme1)


(deftheme face-explorer-test-theme2 "")

(custom-theme-set-faces
 'face-explorer-test-theme2
 '(face-explorer-test-theme-face1 ((t :background "blue")))
 '(face-explorer-test-theme-face2 ((t :underline nil))))

(provide-theme 'face-explorer-test-theme2)

(deftheme face-explorer-test-theme3 "")

(custom-theme-set-faces
 'face-explorer-test-theme3
 '(face-explorer-test-theme-face1 ((t :background "magenta"
                                      :underline t))))

(provide-theme 'face-explorer-test-theme3)


;; --------------------
;; Theme that defines `default'.
;;

(deftheme face-explorer-test-theme4 "")

(custom-theme-set-faces
 'face-explorer-test-theme4
 '(default ((((background dark)) :underline t))))

(provide-theme 'face-explorer-test-theme4)


;; --------------------
;; Theme that defines `default' with both a dark and a light variant.
;;

(deftheme face-explorer-test-theme5 "")

(custom-theme-set-faces
 'face-explorer-test-theme5
 '(default ((((background dark))
             :foreground "yellow" :background "brown")
            (((background light))
             :foreground "maroon3" :background "light green"))))

(provide-theme 'face-explorer-test-theme5)


(ert-deftest face-explorer-test-multiple-theme ()
  "Test theme face definitions."
  (face-explorer-with-fictitious-display-as-frame (selected-frame)
    (should
     (face-explorer-test-faces-plist-equal
      (face-explorer-face-attributes
       'face-explorer-test-theme-face1)
      '(:foreground "red" :background "magenta" :underline t)))
    (should
     (face-explorer-test-faces-plist-equal
      (face-explorer-face-attributes-for-fictitious-display
       'face-explorer-test-theme-face1)
      '(:foreground "red" :background "magenta" :underline t)))
    (should
     (face-explorer-test-faces-plist-equal
      (face-explorer-face-attributes
       'face-explorer-test-theme-face2)
      '(:underline nil)))
    (should
     (face-explorer-test-faces-plist-equal
      (face-explorer-face-attributes-for-fictitious-display
       'face-explorer-test-theme-face2)
      '(:underline nil)))
    ;; Note: No `face-explorer-face-attributes' for `default' as all
    ;; face properties are included.  A
    ;; `face-explorer-test-is-plist-subset' would be needed to test
    ;; it.  In addition, we must check the background mode of the
    ;; frame.
    (setq face-explorer-background-mode 'light)
    (should
     (face-explorer-test-faces-plist-equal
      (face-explorer-face-attributes-for-fictitious-display
       'default)
      '(:foreground "maroon3" :background "light green")))
    ))


;; ------------------------------------------------------------
;; The end
;;

(provide 'face-explorer-test-theme)

;;; face-explorer-test-theme.el ends here
