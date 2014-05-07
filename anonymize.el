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

;; First pass focused only on C source files.  Will generalize once
;; we're functional.

;;; Code:
(require 'cl-lib)

;;;###autoload
(defun anonymize (in-file out-file)
  "Write anonymized contents of IN-FILE to OUT-FILE."
  (interactive "finput file: \nGoutput file: ")
  (copy-file in-file out-file)
  (save-window-excursion
    (find-file out-file)
    (anonymize-region (point-min) (point-max))
    (save-buffer)
    (let ((kill-buffer-hook nil)
          (kill-buffer-query-functions nil))
      (kill-buffer))))

;;;###autoload
(defun anonymize-region (beg end)
  "Anonymize the source code between BEG and END."
  ;; ;; remove all comments
  ;; (anon-comments-region beg end)
  ;; ;; enforce uniform indentation
  ;; (indent-region beg end)
  ;; re-write element (variable and function) names
  (anon-rewrite-elements beg end))

(defun anon-comments-region (beg end)
  (save-excursion
    ;; remove all comments
    (goto-char beg)
    (comment-kill (count-lines beg end))))


;;; C-specific

(defvar anon-C-external-functions-rx
  "^extern\\( \\([^([:space:]]\+\\)\\)\+ ?("
  "Match the names of external functions in a C header file.")

(defvar anon-C-include-rx
  "\#include \\(<\\(.*\\)>\\|\"\\(.*\\)\"\\)"
  "Match included file names.")

(defvar anon-C-include-dirs
  '("/usr/include/"
    "/usr/include/linux")
  "Paths to standard C libraries.")

(defun anon-get-C-external-functions ()
  (save-excursion
    (goto-char (point-min))
    (mapcar
     (lambda (name)                          ; strip any leading stars
       (if (string-match "^\*" name)
           (substring name 1)
           name))
     (cl-loop while (re-search-forward anon-C-external-functions-rx nil t)
             collect (match-string-no-properties 2)))))

(defun anon-C-resolve-include-dir (file)
  (catch 'found
    (mapc (lambda (dir)
            (when (file-exists-p (expand-file-name file dir))
              (throw 'found (expand-file-name file dir))))
          anon-C-include-dirs)))

(defun anon-C-includes ()
  "Return included headers for the current file."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward anon-C-include-rx nil t)
             collect
             (if (match-string-no-properties 2)
                 (anon-C-resolve-include-dir (match-string-no-properties 2))
               (expand-file-name (match-string-no-properties 3)
                                 default-directory)))))

(defun anon-C-reserved-names ()
  (cl-remove-duplicates
   (remove nil
     (mapcan (lambda (f)
               (with-temp-buffer
                 (insert-file-contents f)
                 (anon-get-C-external-functions)))
             (anon-C-includes)))
   :test #'string=))

(defvar anon-C-non-word-chars "-+\*&|!=><;(),[:space:]#{}\r\n")

(defun anon-literalp (string)
  (or
   ;; integer literal
   (string-match "^[[:digit:]]\+$" string)))

(defun anon-collect-elements (&optional beg end)
  (let ((beg (or beg (point-min)))
        (reserved (anon-C-reserved-names))
        (word-rx (format "\\([^%s]\+\\)" anon-C-non-word-chars))
        (vf 'font-lock-variable-name-face))
    (save-excursion
      (goto-char beg)
      (cl-remove-if
       #'anon-literalp
       (cl-remove-duplicates
       (mapcan
        (lambda (word)
          (let ((l (regexp-quote "]"))
                (r (regexp-quote "[")))
            (split-string (replace-regexp-in-string
                           l " " (replace-regexp-in-string
                                  r " " word)) 
                          " " 'omit-nulls)))
        (remove nil
          (cl-loop while (re-search-forward word-rx end t)
                   collect
                   (let ((token (match-string-no-properties 1)))
                     (when (and (save-excursion
                                  (backward-char 1)
                                  (let ((f (face-at-point)))
                                    (or (null f) (equal f vf))))
                                (not (member token reserved)))
                       token)))))
       :test #'string=)))))

(defun anon-rewrite-elements (&optional beg end)
  (interactive "*r")
  (let* ((beg (or beg (point-min)))
         (case-fold-search nil)
         (counter 0)
         (elements (anon-collect-elements)))
    (save-excursion
      (goto-char beg)
      ;; loop through elements, replacing them with new variable names
      (mapc (lambda (el)
              (let ((rx (format "\\(^\\|[%s]\\)\\(%s\\)\\([%s]\\|$\\)"
                                anon-C-non-word-chars
                                el
                                anon-C-non-word-chars))
                    (rep (progn (incf counter) (format "el_%d" counter))))
                (goto-char beg)
                (while (re-search-forward rx end t)
                  (replace-match rep nil 'literal nil 2))))
            elements))))
