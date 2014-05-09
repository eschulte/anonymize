;;; anonymize.el --- source code anonymization

;; Copyright (C) 2013-2014 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Maintainer: Eric Schulte <schulte.eric@gmail.com>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24"))
;; Keywords: source, transform, anonymize, obfuscate
;; URL:

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Currently focused only on C source files.
;; Will probably generalize across languages as the need arises.

;;; Code:
(require 'cl-lib)

;;;###autoload
(defun anonymize (in-file out-file)
  "Write anonymized contents of IN-FILE to OUT-FILE."
  (interactive "finput file: \nGoutput file: ")
  (copy-file in-file out-file)
  (save-window-excursion
    (find-file out-file)
    ;; remove all comments
    (anon-comments)
    ;; enforce uniform indentation
    (indent-region (point-min) (point-max))
    ;; re-write element (variable and function) names
    (anon-rewrite-elements)
    ;; close up shop
    (save-buffer)
    (let ((kill-buffer-hook nil)
          (kill-buffer-query-functions nil))
      (kill-buffer))))

(defun anon-comments ()
  (save-excursion
    ;; remove all comments
    (goto-char (point-min))
    (comment-kill (count-lines (point-min) (point-max)))
    ;; remove all empty lines
    (delete-trailing-whitespace)
    (goto-char (point-min))
    (while (and (re-search-forward "^$" nil t)
                (< (point) (point-max)))
      (delete-char 1))))


;;; C-specific

(defvar anon-C-ext-funs-and-vars-rx
  "^extern\\( \\([^([:space:]]\+\\)\\)\+ ?[(;]"
  "Match the names of external functions or variables in a C header file.")

(defvar anon-C-pound-defines-rx
  "^# ?define \+\\([^[:space:]\n\r]\+\\)"
  "Match the names of macros in a C header file.")

(defvar anon-C-include-rx
  "\#include \\(<\\(.*\\)>\\|\"\\(.*\\)\"\\)"
  "Match included file names.")

(defvar anon-C-include-dirs
  '("/usr/include/"
    "/usr/include/linux"
    "/usr/include/unistring")
  "Paths to standard C libraries.")

(defvar anon-C-non-word-chars "-+\/*&|!=><\?:;(),[:space:]#{}\r\n\.")

(defvar anon-C-builtins '())

(defun anon-get-C-external-symbols ()
  (save-excursion
    (mapcar
     (lambda (name)                          ; strip any leading stars
       (if (string-match "^\*" name)
           (substring name 1)
         name))
     (append
      (progn
        (goto-char (point-min))
        (cl-loop while (re-search-forward anon-C-ext-funs-and-vars-rx nil t)
                 collect (match-string-no-properties 2)))
      (progn
        (goto-char (point-min))
        (cl-loop while (re-search-forward anon-C-pound-defines-rx nil t)
                 collect (match-string-no-properties 1)))))))

(defun anon-C-resolve-include-dir (file)
  (catch 'found
    (mapc (lambda (dir)
            (when (file-exists-p (expand-file-name file dir))
              (throw 'found (expand-file-name file dir))))
          anon-C-include-dirs)
    (warn "couldn't resolve %S in `anon-C-include-dirs'" file)
    nil))

(defun anon-C-includes ()
  "Return included headers for the current file."
  (save-excursion
    (goto-char (point-min))
    (remove nil
      (cl-loop while (re-search-forward anon-C-include-rx nil t)
               collect
               (if (match-string-no-properties 2)
                   (anon-C-resolve-include-dir (match-string-no-properties 2))
                 (expand-file-name (match-string-no-properties 3)
                                   default-directory))))))

(defun anon-C-reserved-names ()
  (append
   anon-C-builtins
   (cl-remove-duplicates
    (remove nil
      (mapcan (lambda (f)
                (if (file-exists-p f)
                    (with-temp-buffer
                      (insert-file-contents f)
                      (anon-get-C-external-symbols))
                  (prog1 nil (warn "couldn't find included file %S" f))))
              (anon-C-includes)))
    :test #'string=)))

(defun anon-literalp (string)
  (or
   ;; integer literal
   (string-match "^[[:digit:]]\+$" string)))

(defun anon-collect-elements ()
  (let* ((reserved (anon-C-reserved-names))
         (word-rx (format "\\([^%s]\+\\)" anon-C-non-word-chars))
         (struct-rx (concat "struct[[:space:]\r\n]\+" word-rx)))
    (cl-flet ((collect (rx face)
                       (goto-char (point-min))
                       (cl-loop while (re-search-forward rx nil t)
                                collect
                                (let ((token (match-string-no-properties 1)))
                                  (when (save-excursion
                                          (backward-char 1)
                                          (let ((f (face-at-point)))
                                            (or (null f) (equal f face))))
                                    token)))))
      (save-excursion
        (cl-remove-if
         #'anon-literalp
         (cl-remove-duplicates
          (remove nil
            (remove-if (lambda (el) (member el reserved))
                       (mapcan
                        (lambda (word)
                          (let ((l (regexp-quote "]"))
                                (r (regexp-quote "[")))
                            (split-string (replace-regexp-in-string
                                           l " " (replace-regexp-in-string
                                                  r " " word)) 
                                          " " 'omit-nulls)))
                        (remove nil
                          (append
                           ;; variable and function names
                           (collect word-rx 'font-lock-variable-name-face)
                           ;; structure names
                           (collect struct-rx 'font-lock-type-face))))))
          :test #'string=))))))

(defvar anon-word-wrap-regex-template
  "\\(^\\|[%s]\\|\\[\\)\\(%s\\)\\([%s]\\|$\\|\\[\\|\\]\\)")

(defun anon-rewrite-elements ()
  (interactive)
  (let* ((case-fold-search nil)
         (counter 0)
         (elements (anon-collect-elements)))
    (save-excursion
      (goto-char (point-min))
      ;; loop through elements, replacing them with new variable names
      (mapc (lambda (el)
              (let ((rx (format anon-word-wrap-regex-template
                                anon-C-non-word-chars
                                (regexp-quote el)
                                anon-C-non-word-chars))
                    (rep (progn (incf counter) (format "el_%d" counter))))
                (goto-char (point-min))
                (while (re-search-forward rx nil t)
                  (replace-match rep nil 'literal nil 2))))
            elements))))

(provide 'anonymize)
