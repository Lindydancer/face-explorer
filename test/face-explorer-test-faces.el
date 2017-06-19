;;; face-explorer-test-faces.el --- Tests for `face-explorer'.

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

;; Test and support functions for the `face-explorer' package.

;;; Code:

(require 'face-explorer)

(defun face-explorer-test-view-buffer-fictitious-display ()
  "Show face-explorer:s view of faces in current buffer.

Create a copy of the current buffer, but with all `face' attributes
replaced with plists containing primitive face attributes."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((source-buffer (current-buffer)))
      (with-current-buffer (get-buffer-create "*FaceExplorerView*")
        (erase-buffer)
        (face-explorer-insert-with-primitive-attributes
         source-buffer
         nil nil t)
        (face-explorer-tooltip-mode 1)
        (display-buffer (current-buffer))))))


(defun face-explorer-test-view-buffer ()
  "Show face-explorer:s view of faces in current buffer.

Create a copy of the current buffer, but with all `face' attributes
replaced with plists containing primitive face attributes."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((source-buffer (current-buffer)))
      (with-current-buffer (get-buffer-create "*FaceExplorerView*")
        (erase-buffer)
        (face-explorer-insert-with-primitive-attributes
         source-buffer
         nil nil )
        (face-explorer-tooltip-mode 1)
        (display-buffer (current-buffer))))))


;; ------------------------------
;; Faces
;;

;; --------------------
;; Foreground
;;

(defface face-explorer-test-faces-red-foreground
  '((t :foreground "red"))
  "Face with red foreground to test face-explorer.")

(defface face-explorer-test-faces-blue-foreground
  '((t :foreground "blue"))
  "Face with blue foreground to test face-explorer.")

(defface face-explorer-test-faces-green-foreground
  '((t :foreground "green"))
  "Face with green foreground to test face-explorer.")


;; --------------------
;; Background
;;

(defface face-explorer-test-faces-red-background
  '((t :background "red"))
  "Face with red background to test face-explorer.")

(defface face-explorer-test-faces-blue-background
  '((t :background "blue"))
  "Face with blue background to test face-explorer.")

(defface face-explorer-test-faces-green-background
  '((t :background "green"))
  "Face with green background to test face-explorer.")


;; --------------------
;; Both fore- and background.
;;

(defface face-explorer-test-faces-red-blue
  '((t :foreground "red" :background "blue"))
  "Test face with both background and foreground.")

(defface face-explorer-test-faces-green-yellow
  '((t :foreground "green" :background "yellow"))
  "Test face with both background and foreground.")


;; --------------------
;; Define the same attribute twice.
;;
;; If the same property occurs more than once, the last one counts.

(defface face-explorer-test-foreground-twice
  '((t :foreground "blue"
       :foreground "green"))
  "Face that define the foreground color twice.")


;; --------------------
;; Various inherited-related faces.
;;

(defface face-explorer-test-faces-underlined
  '((t :underline t))
  "Underlined face to test face-explorer.")

(defface face-explorer-test-faces-inherit-underlined
  '((t :inherit face-explorer-test-faces-underlined))
  "Underlined face to test face-explorer.")


(defface face-explorer-test-faces-inherit
  '((t :inherit font-lock-warning-face))
  "Inherited face to test face-explorer.")

;; The order of the attributes should not matter (this is only an
;; issue for :inherit).

(defface face-explorer-test-faces-inherit-default1
  '((t :inherit default :foreground "green"))
  "Inherited face to test face-explorer.")

(defface face-explorer-test-faces-inherit-default2
  '((t :foreground "green" :inherit default))
  "Inherited face to test face-explorer.")


(defface face-explorer-test-inherit-red-foreground
  '((t :inherit face-explorer-test-faces-red-foreground))
  "Face inheriting `face-explorer-test-faces-red-foreground'
This is used to test remapping.")

(defface face-explorer-test-faces-inherit-list
  '((t :inherit (face-explorer-test-faces-red-foreground
                 face-explorer-test-faces-blue-foreground)))
  "Face inheriting multiple faces.")


;; Inherited values are overwritten, regardless of position in the
;; propoerty list.
(defface face-explorer-test-inherit-in-the-middle
  '((t :foreground "red"
       :inherit face-explorer-test-faces-green-yellow
       :background "red")) "")

;; --------------------
;; Theme faces.
;;

(defface face-explorer-test-faces-with-default
  '((default :background "red")
    (((class color)) :foreground "green"))
  "Test face.")


;; --------------------
;; Obsolete face attribtue (:bold t)
;;

(defface face-explorer-test-bold-default-in-defface '((default :bold t)
                                       (t))
  "Old-style bold in default caluse in defface.")

(defface face-explorer-test-bold-display-req-in-defface
  '((((min-colors 8)) :bold t)
    (t))
  "Old-style bold in display requirement caluse in defface.")

(defface face-explorer-test-bold-last-in-defface '((t :bold t))
  "Old-style bold in match-all caluse in defface.")

(deftheme face-explorer-test-bold
  "Theme to test old-style bold attribtues.")

(defface face-explorer-test-bold-in-default-in-theme nil "")
(defface face-explorer-test-bold-in-display-req-in-theme nil "")
(defface face-explorer-test-bold-in-last-in-theme nil "")

(custom-theme-set-faces
 'face-explorer-test-bold
 '(face-explorer-test-bold-in-default-in-theme ((default :bold t)
                                 (t)))
 '(face-explorer-test-bold-in-display-req-in-theme ((((min-colors 8)) :bold t)
                                     (t)))
 '(face-explorer-test-bold-in-last-in-theme ((t :bold t)))
 ;; The end.
 )

(defface face-explorer-test-bold-in-override nil "")

(face-spec-set 'face-explorer-test-bold-in-override `((t :bold t)))

(defface face-explorer-test-bold-in-new-frames nil "")

(set-face-attribute 'face-explorer-test-bold-in-new-frames nil :bold t)


;; --------------------
;; alias faces
;;

(define-obsolete-face-alias
  'face-explorer-test-aliased-to-default 'default "0.0")

(defface face-explorer-test-inherit-from-aliased-to-default
  '((t :inherit face-explorer-test-aliased-to-default))
  "Face inherits from alias to default.")

;; --------------------
;; All attributes.
;;

;; This demonstrates that :distant-foreground is retained when a face
;; is customized.  (It's not retained in new frames, so I guess it's
;; unintentional.)

(defface face-explorer-test-many-attributes
  '((t
     :width normal
     :height 1.0
     :weight normal
     :slant normal
     :foreground "reg"
     :background "blue"
     :distant-foreground "yellow"
     :strike-through t
     :inverse-video t
     :underline t
     :overline t
     :box 1
     ))
  "Face with many attributes.")

(deftheme face-explorer-test-many-attributes "")

(custom-theme-set-faces
 'face-explorer-test-many-attributes
 '(face-explorer-test-many-attributes ((t))))


;; --------------------
;; A face with both a theme specifier and a future frame attribtue.
;;
;; End result: The future frame attribute takes precedence, in new
;; frames.

(defface face-explorer-test-faces-both-theme-and-future-frame
  '((t :foreground "red"))
  "Face with both a theme-face specifier and a future-frame value.")

(set-face-attribute 'face-explorer-test-faces-both-theme-and-future-frame
                    t                   ; Future frames
                    :foreground "blue")


(defface face-explorer-test-faces-light
  '((((background light)) :foreground "red" :background "blue"))
  "Face defined in light background mode.")

(defface face-explorer-test-faces-dark
  '((((background dark)) :foreground "yellow" :background "brown"))
  "Face defined in dark background mode.")

(defface face-explorer-test-faces-light-and-dark
  '((((background light)) :foreground "red" :background "blue")
    (((background dark))  :foreground "yellow" :background "brown")
    (t  :foreground "grey"))
  "Face defined differently in light and dark background modes.")


;; -------------------------------------------------------------------
;; Define various faces.
;;

(defun face-explorer-test-faces-define-all-kind-of-faces ()
  (interactive)
  (deftheme face-explorer-test-theme "Theme used to test face-explorer.")
  (dolist (fg-use-defface '(nil t))
    (dolist (fg-use-theme '(nil t))
      (dolist (fg-use-default-in-new-frame '(nil t))
        (dolist (fg-use-override '(nil t))
          ;; --------------------
          ;; Foreground
          (let ((face-base-name
                 (concat "face-explorer-test-all-"
                         (if fg-use-default-in-new-frame "f" "-")
                         (if fg-use-theme                "t" "-")
                         (if fg-use-defface              "d" "-")
                         (if fg-use-override             "o" "-"))))
            (let ((fg-defface-spec (if fg-use-defface
                                       '(:foreground "red")
                                     '()))
                  (fg-theme-spec (if fg-use-theme
                                     '(:foreground "green")
                                   '()))
                  (fg-override-spec (if fg-use-override
                                        '(:foreground "yellow")
                                      '())))
              ;; --------------------
              ;; Background
              (dolist (bg-use-defface '(nil t))
                (dolist (bg-use-theme '(nil t))
                  (dolist (bg-use-default-in-new-frame '(nil t))
                    (dolist (bg-use-override '(nil t))
                      (let ((bg-defface-spec (if bg-use-defface
                                                 '(:background "red")
                                               '()))
                            (bg-theme-spec (if bg-use-theme
                                               '(:background "green")
                                             '()))
                            (bg-override-spec (if bg-use-override
                                                  '(:background "yellow")
                                                '())))
                        (let ((face
                               (intern
                                (concat
                                 face-base-name
                                 "-"
                                 (if bg-use-default-in-new-frame "F" "-")
                                 (if bg-use-theme                "T" "-")
                                 (if bg-use-defface              "D" "-")
                                 (if bg-use-override             "O" "-")))))
                          (put face 'face-override-spec nil)
                          (put face 'theme-face nil)
                          (put face 'face-defface-spec nil)

                          (custom-declare-face face
                                               `((t ,@fg-defface-spec
                                                    ,@bg-defface-spec)) "")
                          (when (or fg-use-theme
                                    bg-use-theme)
                            (custom-theme-set-faces
                             'face-explorer-test-theme
                             `(,face ((t (,@fg-theme-spec
                                          ,@bg-theme-spec))))))
                          ;; Note: This change face attributes for
                          ;; future frames, so it must precede the
                          ;; call to `set-face-attribute'.
                          (when (or fg-use-override
                                    bg-use-override)
                            (face-spec-set face `((t ,@fg-override-spec
                                                     ,@bg-override-spec))))
                          (when fg-use-default-in-new-frame
                            (set-face-attribute face nil :foreground "blue"))
                          (when bg-use-default-in-new-frame
                            (set-face-attribute face nil :background "blue"))
                          nil)
                        ;; `nil' is only used as place to put all the
                        ;; close parentheses.
                        nil))))))))))))

;; TODO: Define `face-explorer-test-faces-define-all-kind-of-faces' in
;; terms of this.
;;
;; TODO: Extend it to support display requirements.
(defun face-explorer-test-faces-define-face (face
                                             attrs-defface
                                             attrs-theme
                                             attrs-new-frame
                                             attrs-override)
  "Define face using various sources."
  (put face 'face-override-spec nil)
  (put face 'theme-face nil)
  (put face 'face-defface-spec nil)

  (custom-declare-face face
                       `((t ,@attrs-defface)) "")
  (when attrs-theme
    (custom-theme-set-faces
     'face-explorer-test-theme
     `(,face ((t (,@attrs-theme))))))
  (when attrs-override
    (face-spec-set face `((t ,@attrs-override))))
  (while attrs-new-frame
    (let ((prop (pop attrs-new-frame))
          (value (pop attrs-new-frame)))
      (set-face-attribute face nil prop value))))


;; Define faces where one variant specified one attribute and another
;; the same plus another.  This can be used to verify whether or not a
;; source replaces or merges with another.
(defun face-explorer-test-faces-define-faces-partially-overlapping ()
  (interactive)
  (let ((use-alist '((use-defface   . "D")
                     (use-theme     . "T")
                     (use-new-frame . "F")
                     (use-override  . "O")))
        (attrs-one '(:foreground "red"))
        (attrs-two '(:foreground "blue" :background "yellow")))
    (dolist (one-src '(use-defface use-theme use-new-frame use-override))
      (dolist (two-src '(use-defface use-theme use-new-frame use-override))
        (unless (eq one-src two-src)
          (let ((face
                 (intern
                  (concat
                   "face-explorer-test-onetwo-"
                   (cdr (assq one-src use-alist))
                   (cdr (assq two-src use-alist))))))
            (face-explorer-test-faces-define-face
             face
             (cond ((eq one-src 'use-defface) attrs-one)
                   ((eq two-src 'use-defface) attrs-two)
                   (t '()))
             (cond ((eq one-src 'use-theme) attrs-one)
                   ((eq two-src 'use-theme) attrs-two)
                   (t '()))
             (cond ((eq one-src 'use-new-frame) attrs-one)
                   ((eq two-src 'use-new-frame) attrs-two)
                   (t '()))
             (cond ((eq one-src 'use-override) attrs-one)
                   ((eq two-src 'use-override) attrs-two)
                   (t '())))))))))


(defun face-explorer-test-faces-remove-face (face)
  "Remove FACE."
  (let ((pair (assq face face-new-frame-defaults)))
    (when pair
      (setq face-new-frame-defaults
            (delq pair face-new-frame-defaults)))))


(defvar face-explorer-test-all-faces-exclude-list
  '(;; Initial frame contains a :distant-foreground value. (This could
    ;; be a bug in Emacs.)
    next-error
    region
    rectangle-preview
    face-explorer-test-many-attributes
    ;; My normal font doesn't provide a bold variant, whereas a
    ;; fictitious graphical display do, so these will be rendered
    ;; differently.
    face-explorer-feature-bold
    face-explorer-feature-bold-no
    face-explorer-feature-bold-yes
    ;; Designed to different in current and future frames.
    face-explorer-test-faces-both-theme-and-future-frame))


;; TODO: Do something better than ignoring faces that inherit from
;; `default'.

(ert-deftest face-explorer-test-all-faces ()
  "Test that faces are equal in current and fictitious displays."
  ;; In batch mode, faces aren't defined normally. E.g. there is no
  ;; background mode, `:weight bold' doesn't seem to be defined etc.
  ;;
  ;; Simply ignore for now. It would be a lot of work to
  ;; reverse-engineer how a batch frame really work.
  (unless noninteractive
    (face-explorer-test-faces-define-faces-partially-overlapping)
    (face-explorer-test-faces-define-all-kind-of-faces)
    (face-explorer-with-fictitious-display-as-frame (selected-frame)
      (let ((attributes (mapcar #'car custom-face-attributes)))
        (dolist (face (face-list))
          (unless (memq face face-explorer-test-all-faces-exclude-list)
            ;; TODO: Replace with `face-explorer-face-attributes'?
            (let ((plist '()))
              (dolist (attr attributes)
                (unless (eq attr :inherit)
                  (let ((value (face-attribute face attr nil t)))
                    (unless (eq value 'unspecified)
                      (push value plist)
                      (push attr plist)))))
              (let ((pair
                     (face-explorer-face-xattributes-for-fictitious-display
                      face)))
                (unless (car pair)
                  (should
                   (face-explorer-test-faces-plist-equal
                    plist
                    (cdr pair)
                    ;; Not used, only supplied to make it easier to
                    ;; determine which face failed.
                    face)))))))))))


;; -------------------------------------------------------------------
;; Test support functions.
;;

;; Oddly enough, Emacs doesn't provide an `oddp' function.  (Well, it
;; does, but only in a Common Lisp compatibility package.)
(defun face-explorer-test-faces-oddp (integer)
  "Non-nil when INTEGER is odd."
  (eq (logand integer 1) 1))

(defun face-explorer-test-faces-plist-equal-explainer (plist1 plist2
                                                              &rest _ignored)
  "Ert explainer function for `face-explorer-test-faces-plist-equal'."
  (cond ((face-explorer-test-faces-oddp (length plist1))
         '(first-argument-has-odd-length))
        ((face-explorer-test-faces-oddp (length plist2))
         '(second-argument-has-odd-length))
        (t
         (catch 'ret
           (let ((plist1-copy plist1))
             (while plist1-copy
               (let ((prop (pop plist1-copy))
                     (value1 (pop plist1-copy)))
                 (let ((rest2 (plist-member plist2 prop)))
                   (unless rest2
                     (throw 'ret `(property ,prop missing in second argument)))
                   (unless (equal value1 (nth 1 rest2))
                     (throw 'ret `(value of ,prop differs))))))
             (while plist2
               (let ((prop (pop plist2)))
                 (pop plist2)
                 (let ((rest1 (plist-member plist1 prop)))
                   (unless rest1
                     (throw 'ret
                            `(property ,prop missing in first argument)))))))
           t))))


(defun face-explorer-test-faces-plist-equal (plist1 plist2 &rest _ignored)
  "Non-nil if PLIST1 and PLIST2 are equal, order ignored."
  (if (eq (face-explorer-test-faces-plist-equal-explainer plist1 plist2) t)
      t
    nil))

(put 'face-explorer-test-faces-plist-equal
     'ert-explainer
     #'face-explorer-test-faces-plist-equal-explainer)

(ert-deftest face-explorer-test-faces-plist-equal ()
  "Selftest for `face-explorer-test-faces-plist-equal'."
  (should (face-explorer-test-faces-plist-equal '() '()))
  (should (face-explorer-test-faces-plist-equal '(:alpha 1) '(:alpha 1)))
  (should (face-explorer-test-faces-plist-equal '(:alpha 1 :beta 2)
                                                '(:alpha 1 :beta 2)))
  (should (face-explorer-test-faces-plist-equal '(:alpha 1 :beta 2)
                                                '(:beta 2 :alpha 1)))
  ;; Note: Uncomment to check explainer.
  ;; (should (face-explorer-test-faces-plist-equal '(x) '()))
  ;; (should (face-explorer-test-faces-plist-equal '(:alpha 1) '()))
  ;; (should (face-explorer-test-faces-plist-equal '() '(:alpha 1)))
  ;; (should (face-explorer-test-faces-plist-equal '(:alpha 2) '(:alpha 1)))
  )


;; -------------------------------------------------------------------
;; Face attribtues.
;;

(ert-deftest face-explorer-test-face-attributes ()
  "Test for `face-explorer-face-xattributes'."
  ;; Check that the "inherits from default" flag is set.
  (should (equal (car (face-explorer-face-xattributes-for-fictitious-display
                       'default))
                 t))
  (should (equal (car (face-explorer-face-xattributes
                       'default))
                 t))
  (should (equal (car (face-explorer-face-xattributes-for-fictitious-display
                       'face-explorer-test-aliased-to-default))
                 t))
  (should (equal (car (face-explorer-face-xattributes
                       'face-explorer-test-aliased-to-default))
                 t))
  ;; Check that the "inherits from default" flag is set, and that
  ;; properties are correct.
  (should (equal (face-explorer-face-xattributes-for-fictitious-display
                  'face-explorer-test-faces-inherit-default1)
                 '(t . (:foreground "green"))))
  (should (equal (face-explorer-face-xattributes
                  'face-explorer-test-faces-inherit-default1)
                 '(t . (:foreground "green"))))
  (should (equal (face-explorer-face-xattributes-for-fictitious-display
                  'face-explorer-test-inherit-from-aliased-to-default)
                 '(t)))
  (should (equal (face-explorer-face-xattributes
                  'face-explorer-test-inherit-from-aliased-to-default)
                 '(t)))
  nil)


;; -------------------------------------------------------------------
;; Face text properties.
;;

(ert-deftest face-explorer-test-faces-face-prop ()
  "Ert test for `face-explorer-face-prop-attributes'."
  (with-temp-buffer
    ;; For `face-remapping-alist'.
    (face-explorer-list-face-prop-examples-mode)
    (face-explorer-example-each-face-prop tripple count
      (let ((expected (nth 2 tripple)))
        (unless (assq :distant-foreground custom-face-attributes)
          (setq expected
                (face-explorer-plist-delete :distant-foreground expected)))
        (should (face-explorer-test-faces-plist-equal
                 expected
                 (face-explorer-face-prop-attributes (nth 1 tripple))
                 (nth 1 tripple)
                 count))))))


(ert-deftest face-explorer-test-faces-face-prop-remapping ()
  (let ((face-remapping-alist
         '((face-explorer-test-faces-red-foreground
            . face-explorer-test-faces-blue-foreground))))
    ;; --------------------
    ;; `f-e-face-attributes' should not be affected by remapping
    ;; because it is used as a subroutine to the remapping code to
    ;; find a original face attributes and this corresponds to how
    ;; `face-attribute' work.
    (should (face-explorer-test-faces-plist-equal
             (face-explorer-face-attributes
              'face-explorer-test-faces-red-foreground)
             '(:foreground "red")))
    (should (face-explorer-test-faces-plist-equal
             (face-explorer-face-attributes
              'face-explorer-test-inherit-red-foreground)
             '(:foreground "red")))
    ;; --------------------
    ;; `face' text properties should be affected.
    (should (face-explorer-test-faces-plist-equal
             (face-explorer-face-prop-attributes
              'face-explorer-test-faces-red-foreground)
             '(:foreground "blue")))
    (should (face-explorer-test-faces-plist-equal
             (face-explorer-face-prop-attributes
              'face-explorer-test-inherit-red-foreground)
             '(:foreground "blue")))
    (should (face-explorer-test-faces-plist-equal
             (face-explorer-face-prop-attributes
              "face-explorer-test-faces-red-foreground")
             '(:foreground "blue")))))


(ert-deftest face-explorer-test-faces-find-duplicates ()
  "Ensure there are no duplicated in `face-explorer-test-faces-spec-list'."
  (let ((seen-list '()))
    (face-explorer-example-each-face-prop tripple count
      (let ((key (nth 1 tripple)))
        (should-not (member key seen-list))
        (push key seen-list)))))


;; -------------------------------------------------------------------
;; `face-explorer-context-colors'
;;

(ert-deftest face-explorer-test-faces-context-colors ()
  ""
  (let ((face-explorer-background-mode 'light))
    ;; --------------------
    ;; If the face doesn't make a difference between light and dark
    ;; background, it's not suited to be used for sample texts.
    (let ((face-explorer-light-mode-colors
           '(face-explorer-test-faces-red-blue ("F" . "B"))))
      (should (face-explorer-test-faces-plist-equal
               (face-explorer-context-colors)
               '(:foreground "F" :background "B"))))
    ;; --------------------
    ;; If the face doesn't make a difference between light and dark
    ;; background, but doesn't provide fallback colors, use it anyway.
    (let ((face-explorer-light-mode-colors
           '(face-explorer-test-faces-red-blue)))
      (should (face-explorer-test-faces-plist-equal
               (face-explorer-context-colors)
               '(:foreground "red" :background "blue"))))
    ;; --------------------
    ;; Face only defined in light background.
    (let ((face-explorer-light-mode-colors
           '(face-explorer-test-faces-light ("F" . "B"))))
      (should (face-explorer-test-faces-plist-equal
               (face-explorer-context-colors)
               '(:foreground "red" :background "blue"))))
    ;; --------------------
    ;; Face defined differently in light and dark background.
    (let ((face-explorer-light-mode-colors
           '(face-explorer-test-faces-light-and-dark ("F" . "B"))))
      (should (face-explorer-test-faces-plist-equal
               (face-explorer-context-colors)
               '(:foreground "red" :background "blue"))))))


;; ------------------------------------------------------------
;; The end
;;

(provide 'face-explorer-test-faces)

;;; face-explorer-test-faces.el ends here
