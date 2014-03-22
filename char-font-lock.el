;;; char-font-lock.el -- Highlight bad whitespace and out-of-place characters.

;; Copyright (C) 2014 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: languages, faces
;; Version: 0.0.0
;; URL: https://github.com/Lindydancer/char-font-lock

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

;;{{{ Documentation

;; *Char Font Lock* is an Emacs package that highlight bad whitespace
;; and out-of-place characters.
;;
;; Char Font Lock is implemented as two minor modes:
;; `char-font-lock-mode', which can be applied to an individual
;; buffer, and `char-font-lock-global-mode' which automatically
;; enables the mode in all existing and future buffers.

;; What is highlighted:
;;
;; Currently, the following are highlighted:
;;
;; * Correct tab characters are highlighted using a neutral color, to
;;   indicate that they are there
;;
;; * Incorrect tabs, i.e. tabs not first on the line or any tab in
;;   case `indent-tabs-mode' isn't active
;;
;; * Empty lines at the end of the buffer
;;
;; * The last line, if it ends without a newline
;;
;; * End of line whitespace. (Technically, this is not highlighted using
;;   a font-lock keyword, instead the built-in feature
;;   `show-trailing-whitespace' is used.)

;; Usage:
;;
;; Place the source file in a directory in the load path. Add the
;; following lines to an appropriate init file:
;;
;;    (require 'char-font-lock)
;;
;; Activate this package by Customize, or by placing the following line
;; into the appropriate init file:
;;
;;    (char-font-lock-global-mode 1)
;;
;; This package use Font Lock mode, so `font-lock-mode' or
;; `global-font-lock-mode' must be enabled (which it is by default).
;;
;; Customization:
;;
;; The following variables can be modified to fine-tune Char Font Lock:
;;
;; * `char-font-lock-modes' -- For major modes that are members of
;; this list, or are derived from members in this list, Char Font Lock
;; Global mode will be enabled. As `prog-mode' and `text-mode'
;; initially are members, this mean that Char Font Lock is enabled for
;; most major modes, with the exception of special major modes like
;; `help-mode'. Some ancient third-party major modes are not derived
;; from the base modes provided by Emacs, in this case you can
;; explicitly add them to this list (alternatively, politely ask their
;; authors to take a step into the modern world).
;;
;; * `char-font-lock-enabled-features-list' -- Control which highlight
;; rules should be applied to a major mode. For example, with the
;; default settings, end-of-file whitespace for
;; `lisp-interaction-mode' (used by the scratch buffer) is not
;; highlighted.

;; Example:
;;
;; Below is a screenshot of a sample file, demonstrating the effect of
;; this package:
;;
;; ![See doc/demo.png for screenshot of Char Font Lock mode](doc/demo.png)

;;}}}

;;; Code:

;;{{{ Dependencies

(eval-when-compile
  (require 'cl))

;;}}}
;;{{{ Variables

(defgroup char-font-lock nil
  "Highlight bad whitespace and out-of-place characters."
  :group 'faces)


(defface char-font-lock-ok-tab
  '((((class color) (background light)) (:background "DarkSeaGreen1")))
  "Face for correct tabs in `char-font-lock-mode'."
  :group 'char-font-lock)


(defvar char-font-lock-ok-tab-face 'char-font-lock-ok-tab
  "Face to use to highlight correct TAB characters.")


(defvar char-font-lock-bad-tab-face 'trailing-whitespace
  "Face to use to highlight suspicious TAB characters.")


(defvar char-font-lock-nonascii-face 'trailing-whitespace)


(defcustom char-font-lock-modes
  '(prog-mode
    text-mode)
  "List of major modes in which Char Font Lock Global mode should be enabled.

Char Font Lock is enabled for buffers whose major mode is a
member of this list, or is derived from a member in the list."
  :group 'char-font-lock
  :type '(repeat symbol))


(defvar char-font-lock-all-features
  '(end-of-line-whitespace
    end-of-file-whitespace
    missing-end-of-file-newline
    ok-tab
    bad-tab
    nonascii))


(defcustom char-font-lock-enabled-features-list
  '((lisp-interaction-mode (- end-of-file-whitespace
                              missing-end-of-file-newline)))
  "List of (MODE FEATURES) controlling enabled Char Font Lock features.

An entry in the list applies when the major mode is MODE, or is
derined from MODE.

FEATURES is a list of features (see
`char-font-lock-all-features'), possibly prepended with `+' or
`-'. Unless prepended, the list is the enabled features. When
prepended with a `+', the features are added to the current set
of features, when prepended with a `-', they are removed. The
list is traversed in order.")


;;}}}
;;{{{ The modes

;;;###autoload
(define-minor-mode char-font-lock-mode
  "Minor mode that highlight bad whitespace and out-of-place characters."
  nil
  nil
  nil
  :group 'char-font-lock
  (if char-font-lock-mode
      (char-font-lock-font-lock-add-keywords)
    (char-font-lock-font-lock-remove-keywords))
  (when font-lock-mode
    (font-lock-fontify-buffer)))

;;;###autoload
(define-global-minor-mode char-font-lock-global-mode char-font-lock-mode
  (lambda ()
    (when (apply 'derived-mode-p char-font-lock-modes)
      (char-font-lock-mode 1)))
  :group 'char-font-lock)

;;}}}
;;{{{ Match functions

(defun char-font-lock-ok-tab-p ()
  "True when character at point is OK location for a TAB character."
  (and indent-tabs-mode
       (save-excursion
         (skip-chars-backward "\t")
         (bolp))))


;; Note: This does not work to 100% due to re-fontification problems.
;; For example, the buffer ends with a number of empty lines and a
;; character is types on the last line and then deleted. In ths case,
;; the fontification is first removed (correctly) but then not
;; reapplied, since font-lock don't back up enough when it reapplies
;; the rule, so the search fail.
(defun char-font-lock-match-empty-lines-at-eob (limit)
  (let ((p (point)))
    (goto-char (point-max))
    (let ((res (and (> (abs (skip-chars-backward "\n")) 1)
                    (>= limit (point))
                    (> (point) p))))
      (if res
          (progn
            ;; Fake match-data, used by post-command hook to place
            ;; point.
            (set-match-data (list (point) (+ (point) 1)))
            ;; Don't highlight the first newline.
            (forward-char))
        ;; Don't move point when search failed.
        (goto-char p))
      res)))


(defun char-font-lock-match-bad-tab (limit)
  (let (res)
    (while (and (setq res (re-search-forward "\C-i" limit t))
                (char-font-lock-ok-tab-p)))
    res))


(defun char-font-lock-match-ok-tab (limit)
  (let (res)
    (while (and (setq res (re-search-forward "\C-i" limit t))
                (not (char-font-lock-ok-tab-p))))
    res))


(defun char-font-lock-match-missing-new-line-at-eob (limit)
  "Match line without newline at end of buffer."
  (if (or (eobp)
          (< limit (point-max)))
      nil
    (goto-char (point-max))
    (if (or (eq (point-min) (point-max))
            (eq (char-before) ?\n))
        nil
      (set-match-data (list (line-beginning-position) (point)))
      t)))


(defun char-font-lock-enabled-features ()
  "Return list of enables features in current buffer."
  (let ((features char-font-lock-all-features))
    (dolist (entry char-font-lock-enabled-features-list)
      (let ((mode (nth 0 entry))
            (spec (nth 1 entry)))
        (when (derived-mode-p (car entry))
          (cond ((eq (car-safe spec) '+)
                 (setq features (union features (cdr spec))))
                ((eq (car-safe spec) '-)
                 (setq features (set-difference features (cdr spec))))
                (t
                 (setq features spec))))))
    features))


(defun char-font-lock-keywords ()
  (let ((features (char-font-lock-enabled-features))
        (res '()))
    (when (memq 'ok-tab features)
      (push '(char-font-lock-match-ok-tab
              (0 char-font-lock-ok-tab-face))
            res))
    (when (memq 'bad-tab features)
      (push '(char-font-lock-match-bad-tab
              (0 char-font-lock-bad-tab-face))
            res))
    (when (memq 'nonascii features)
      (push '("[[:nonascii:]]+"
              (0 char-font-lock-nonascii-face append))
            res))
    (when (memq 'end-of-file-whitespace features)
      ;; Note: Reverse order, the `prepare' rules is executed first.
      (push '(char-font-lock-match-empty-lines-at-eob
              ("\n"
               (point-max)
               (goto-char (match-end 0))
               (0 char-font-lock-bad-tab-face append)))
            res))
    (when (memq 'missing-end-of-file-newline features)
      (push '(char-font-lock-match-missing-new-line-at-eob
              (0 char-font-lock-bad-tab-face append))
            res))
    res))


(defvar char-font-lock--installed-keywords nil)
(defvar char-font-lock--show-trailing-whitespace nil)


(defun char-font-lock-font-lock-add-keywords ()
  "Install Char Font Lock mode keywords into current buffer."
  (let ((keywords (char-font-lock-keywords)))
    (set (make-local-variable 'char-font-lock--show-trailing-whitespace)
         show-trailing-whitespace)
    (set (make-local-variable 'char-font-lock--installed-keywords) keywords)
    (when (memq 'end-of-line-whitespace (char-font-lock-enabled-features))
      (setq show-trailing-whitespace t))
    (set (make-local-variable 'font-lock-multiline) t)
    (font-lock-add-keywords nil keywords t)))


(defun char-font-lock-font-lock-remove-keywords ()
  "Remove Char Font Lock mode keywords in current buffer."
  (setq show-trailing-whitespace char-font-lock--show-trailing-whitespace)
  (font-lock-remove-keywords nil char-font-lock--installed-keywords))


;;}}}

;;{{{ The end

(provide 'char-font-lock)

;;}}}

;;; char-font-lock.el ends here.
