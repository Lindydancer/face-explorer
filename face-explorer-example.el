;;; face-explorer-example.el --- Example using face-explorer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Anders Lindgren

;; Author: Anders Lindgren
;; Created: 2026-06-14

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an example if using the `face-explorer' library to convert
;; a syntax highlighted buffer to another format.
;;
;; The result is displayed with the highlighting presented as
;; `{{{ATTRIBUTES|||STR}}}'.
;;
;; For example, the following C program:
;;
;;     int x;
;;
;; Could be rendered as:
;;
;;     {{{(:foreground "green")|||int}}} {{{(:foreground "red")|||x}}};
;;
;; Interactive usage:
;;
;; Run `M-x face-explorer-example-display-highlighting RET' to view
;; the highlighting of the current buffer.
;;
;; Batch usage:
;;
;; This can be executed in batch mode. For example, the following can
;; be used to view the file `file.ext':
;;
;;     emacs --batch -L PATH-TO-FACE-EXPLORER
;;           -l face-explorer-example.el
;;           file.ext
;;           -f face-explorer-example-batch

;; Why is the face explorer "fictitious display" used?
;;
;; In batch mode, Emacs doesn't provide face information. However, the
;; face example fictitious display system allows it to retrieve the
;; face information anyway.
;;
;; By default, the fictitious display is set up to match a typical
;; grahical display, so the result of the batch run should be the same
;; as when launched in a GUI.
;;
;; Inside `face-explorer-example-display-highlighting', the fictitious
;; display is set up to match that of the current frame using
;; `face-explorer-with-fictitious-display-as-frame'.

;;; Code:

(require 'face-explorer)

(defun face-explorer-example-print-highlighting (&optional dest)
  "Print the current buffer with face information to DEST.

Face information is printed as {{{FACE-ATTRIBUTES|||TEXT}}}."
  ;; Ensure that the entire buffer is highlighted.
  (ignore-errors
    (face-explorer-fontify-buffer))
  ;; Loop over the input buffer.
  (let ((pos (point-min))
        (last-pos (point-min)))
    (while (setq pos (face-explorer-next-face-property-change pos))
      (let ((attributes
             (face-explorer-face-attributes-for-fictitious-display-at
              last-pos))
            (str (buffer-substring-no-properties last-pos pos)))
        (if attributes
            (princ (format "{{{%S|||%s}}}" attributes str) dest)
          (princ str dest)))
      (setq last-pos pos))))


(defun face-explorer-example-display-highlighting ()
  "Display the current buffer with face information.

Face information is displayed as {{{FACE-ATTRIBUTES|||TEXT}}}."
  (interactive)
  (let ((dest (get-buffer-create "*FaceExplorerExample*")))
    (with-current-buffer dest
      (erase-buffer))
    (face-explorer-with-fictitious-display-as-frame (selected-frame)
      (face-explorer-example-print-highlighting dest))
    (display-buffer dest)))


(defun face-explorer-example-batch ()
  "Print the current buffer with face information in batch mode."
  ;; Normally, font-lock mode isn't be enabled in batch mode.
  (let ((noninteractive nil))
    (font-lock-mode 1))
  (face-explorer-example-print-highlighting))


(provide 'face-explorer-example)

;;; face-explorer-example.el ends here
