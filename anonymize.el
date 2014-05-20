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

;; Currently focused only on C and OCaml source files.
;; Will probably generalize across languages as the need arises.

;;; Code:
(require 'cl-lib)


(defvar *anon-current-file* nil
  "Bound locally for better error messages.")

(defvar anon-supported-modes '(c-mode tuareg-mode)
  "Major modes currently supported for anonymization.")

;;;###autoload
(defun anonymize (in-file out-file)
  "Write anonymized contents of IN-FILE to OUT-FILE."
  (interactive "finput file: \nGoutput file: ")
  (copy-file in-file out-file)
  (let ((*anon-current-file* out-file))
    (save-window-excursion
      (find-file out-file)
      (font-lock-mode 1)
      (unless (member major-mode anon-supported-modes)
        (error "Anonymization not supported for %s" major-mode))
      (message "anonymizing for %S with %S" major-mode font-lock-mode)
      ;; remove all comments
      (anon-comments)
      ;; re-write element (variable and function) names
      (anon-rewrite-elements)
      ;; enforce uniform indentation
      (indent-region (point-min) (point-max))
      ;; close up shop
      (save-buffer)
      (let ((kill-buffer-hook nil)
            (kill-buffer-query-functions nil))
        (kill-buffer)))))

;;;###autoload
(defun anonymize-directory (directory)
  "Anonymize all source files in DIRECTORY."
  (interactive "Danonymize files in: ")
  (mapcar (lambda (in-file)
            (let* ((ext (file-name-extension in-file))
                   (postfix (cond
                             ((string= ext "c") "-anonymized")
                             ((string= ext "ml") "_anonymized")))
                   (rx (format "%s\.%s$" postfix ext)))
              (if (string-match rx in-file)
                  in-file
                (let ((out-file (concat (file-name-sans-extension in-file)
                                        postfix "." ext)))
                  (anonymize in-file out-file)
                  out-file))))
          (directory-files directory t "^[^\.].*\.\\(c\\|ml\\)$")))

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

(defvar anon-non-word-chars "-+\/\\%*&|!^=><\?:;(),[:space:]#{}\r\n\.")

(defun anon-rewrite-elements ()
  (interactive)
  (let* ((case-fold-search nil)
         (counter 0)
         (fmt (ecase major-mode
                (c-mode "_%d")
                (tuareg-mode "a%d")))
         (elements (ecase major-mode
                     (c-mode (anon-C-collect-elements))
                     (tuareg-mode (anon-ocaml-collect-elements)))))
    (when (null elements)
      (warn "no elements to anonymize in %s" *anon-current-file*))
    (save-excursion
      (goto-char (point-min))
      ;; loop through elements, replacing them with new variable names
      (mapc (lambda (el)
              (let ((rx (format anon-word-wrap-regex-template
                                anon-non-word-chars
                                (regexp-quote el)
                                anon-non-word-chars))
                    (rep (progn (incf counter) (format fmt counter))))
                (goto-char (point-min))
                (while (re-search-forward rx nil t)
                  (unless (save-excursion
                            (save-match-data
                              (backward-char 1)
                              (or
                               (anon-on-a-c-number)
                               ;; we're in a string or an #include argument
                               (equal (face-at-point)
                                      'font-lock-string-face)
                               ;; we're in a comment
                               (equal (face-at-point)
                                      'font-lock-comment-face))))
                    (replace-match rep nil 'literal nil 2)))))
            elements))))

(defun anon-on-a-number ()
  (ecase major-mode
    (c-mode (anon-on-a-c-number))
    (:otherwise nil)))


;;; C-specific

(defvar anon-C-ext-funs-and-vars-rx
  "^extern\\( \\([^([:space:]]\+\\)\\)\+ ?[(;]"
  "Match the names of external functions or variables in a C header file.")

(defvar anon-C-pound-defines-rx
  "^# *define \+\\([^[:space:]\n\r]\+\\)"
  "Match the names of macros in a C header file.")

(defvar anon-C-typedef-rx
  "typedef[[:space:]\*]\+\\(struct[[:space:]\*]\+\\)?[^[:space:]\*]\+[[:space:]\*]\+\\([^[:space:]\*;]\+\\)"
  "Match the names of types defined in a C header file.")

(defvar anon-C-include-rx
  "\#include[[:space:]]*\\(<\\(.*\\)>\\|\"\\(.*\\)\"\\)"
  "Match included file names.")

(defvar anon-C-num-rx
  "^\\([:digit:]\+\.e[:digit:]\+]\\|[[:digit:]]\+[[:digit:]\.A-Fa-f]\+\\|0x[0-9A-Fa-f]\+\\)$"
  "Match C numbers which might look like words.")

(defvar anon-C-include-dirs
  '("/usr/include/"
    "/usr/include/linux"
    "/usr/include/unistring")
  "Paths to standard C libraries.")

(defun anon-get-C-external-symbols ()
  (cl-flet ((collect (rx match)
                     (goto-char (point-min))
                     (cl-loop while (re-search-forward rx nil t)
                              collect (match-string-no-properties match))))
    (save-excursion
      (mapcar
       (lambda (name)                          ; strip any leading stars
         (if (string-match "^\*" name)
             (substring name 1)
           name))
       (append
        (collect anon-C-ext-funs-and-vars-rx 2)
        (collect anon-C-pound-defines-rx 1)
        (collect anon-C-typedef-rx 2))))))

(defun anon-C-resolve-include-dir (file)
  (catch 'found
    (mapc (lambda (dir)
            (when (file-exists-p (expand-file-name file dir))
              (throw 'found (expand-file-name file dir))))
          anon-C-include-dirs)
    (warn "couldn't resolve %S in `anon-C-include-dirs'" file)
    nil))

(defmacro in-file (file &rest body)
  (declare (indent 1))
  ;; - check if file is already being visited
  ;; - ensure mode is set
  (let ((tempvar (make-symbol "file")))
    `(let* ((,tempvar ,file)
	    (visited-p (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (point (point)) to-be-removed)
       (save-window-excursion
         (when ,tempvar (find-file ,tempvar))
         (setq to-be-removed (current-buffer))
         (goto-char (point-min))
         (unwind-protect (progn ,@body)
           (unless visited-p (kill-buffer to-be-removed))
           (goto-char point))))))

(defvar anon-C-includes-stop nil
  "Track current and past stack of included files.")

(defvar anon-C-includes-mapping nil
  "Map include file names to lists of included files.")

(defmacro anon-memoized (key map &rest body)
  (declare (indent 2))
  `(or (assoc ,key ,map)
       (car (push (cons ,key (progn ,@body)) ,map))))

(defun anon-C-includes-single-file (file)
  (anon-memoized file anon-C-includes-mapping
    (in-file file
      (remove nil
        (cl-loop while (re-search-forward anon-C-include-rx nil t)
                 collect
                 (if (match-string-no-properties 2)
                     (anon-C-resolve-include-dir
                      (match-string-no-properties 2))
                   (expand-file-name (match-string-no-properties 3)
                                     default-directory)))))))

(defun annon-C-includes- (file)
  (unless (member file anon-C-includes-stop)
    (push file anon-C-includes-stop)
    (cons file (mapcan #'annon-C-includes-
                       (anon-C-includes-single-file file)))))

(defun anon-C-includes ()
  "Return included headers for the current file."
  (let ((anon-C-includes-stop nil))
    (cdr (annon-C-includes- (buffer-file-name)))))

(defun anon-C-names-from-includes (includes)
  (cl-remove-duplicates
   (remove nil
     (mapcan (lambda (f)
               (if (file-exists-p f)
                   (with-temp-buffer
                     (insert-file-contents f)
                     (anon-get-C-external-symbols))
                 (prog1 nil (warn "couldn't find included file %S" f))))
             includes))
   :test #'string=))

(defvar anon-C-builtins
  `("main" "EXIT_SUCCESS" "EXIT_FAILURE" "EINVAL" "errno"
    ;; types
    "char" "double" "float" "int" "long" "ptrdiff_t" "short" "signed"
    "size_t" "unsigned" "void"
    ,@(anon-C-names-from-includes
       (mapcar #'anon-C-resolve-include-dir
               (list "stdlib.h" "stdio.h" "stddef.h" "string.h" "unistd.h"))))
  "Builtin C functions.
Recognized by linkers even if the header isn't included, so we
should too.")

(defun anon-C-reserved-names ()
  (append
   anon-C-builtins
   (anon-C-names-from-includes (anon-C-includes))))

(defun anon-literalp (string)
  (or
   ;; integer literal
   (string-match "^[[:digit:]]\+$" string)))

(defun anon-C-collect-elements ()
  (let ((reserved (anon-C-reserved-names))
        (word-rx (format "\\([^%s]\+\\)" anon-non-word-chars)))
    (cl-flet ((collect (rx &rest faces)
                       (goto-char (point-min))
                       (cl-loop while (re-search-forward rx nil t)
                                collect
                                (let ((token (match-string-no-properties 1)))
                                  (when (save-excursion
                                          (goto-char (match-beginning 1))
                                          (let ((f (face-at-point)))
                                            (or (null f) (member f faces))))
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
                           ;; variable type and function names
                           (collect word-rx
                                    'font-lock-variable-name-face
                                    'font-lock-function-name-face
                                    'font-lock-type-face))))))
          :test #'string=))))))

(defvar anon-word-wrap-regex-template
  "\\(^\\|[%s]\\|\\[\\)\\(%s\\)\\([%s]\\|$\\|\\[\\|\\]\\)")

(defun anon-on-a-c-number ()
  (let ((wd (buffer-substring
             (save-excursion
               (re-search-backward
                "[[:space:]\r\n(),]" nil t)
               (+ 1 (point)))
             (point))))
    (and wd
         (let ((case-fold-search t))
           (string-match anon-C-num-rx wd)))))


;;; ocaml-specific
(defun anon-ocaml-collect-elements ()
  ;; This may be required to get tuareg-mode to actually do
  ;; fontification.
  (sit-for 0.01)
  (let ((reserved (list "compare" "partition")))
    (cl-remove-if (lambda (el) (member el reserved))
                  (append
                   (let ((word-rx (format "\\([^%s]\+\\)" anon-non-word-chars)))
                     (cl-remove-duplicates
                      (remove nil
                        ;; variable type and function names
                        (save-excursion
                          (goto-char (point-min))
                          (cl-loop
                           while (re-search-forward word-rx nil t)
                           collect
                           (let ((token (match-string-no-properties 1)))
                             (when (save-excursion
                                     (goto-char (match-beginning 1))
                                     (member (face-at-point)
                                             '(font-lock-variable-name-face
                                               font-lock-function-name-face
                                               font-lock-type-name-face)))
                               token)))))
                      :test #'string=))
                   (anon-ocaml-collect-types-w-fields)
                   (anon-ocaml-collect-modules)))))

(defun anon-ocaml-collect-types-w-fields ()
  (let ((type-rx (format "type \\([^%s]\+\\) =" anon-non-word-chars))
        (fields-rx "[[:space:]]*{\\([^}]\+\\)}[[:space:]]*;;")
        (reserved (list "t")))
    (goto-char (point-min))
    (cl-remove-if
     (lambda (el) (member el reserved))
     (apply
      #'append
      (cl-loop
       while (re-search-forward type-rx nil t)
       collect
       (when (save-excursion
               (goto-char (match-beginning 1))
               (equal (face-at-point) 'font-lock-type-face))
         (cons (match-string-no-properties 1)
               ;; possibly collect field names
               (save-match-data
                 (when (looking-at fields-rx)
                   (let ((body (match-string-no-properties 1))
                         (space "[[:space:]]*"))
                     (mapcar (lambda (f) (car (split-string f ":" 'omit space)))
                             (split-string body ";" 'omit space))))))))))))

(defun anon-ocaml-collect-modules ()
  (let ((module-rx (format "module \\([^%s]\+\\) =" anon-non-word-chars))
        (reserved nil))
    (goto-char (point-min))
    (cl-remove-if
     (lambda (el) (member el reserved))
     (cl-loop while (re-search-forward module-rx nil t)
              when (save-excursion (goto-char (match-beginning 1))
                                   (equal (face-at-point) 'font-lock-type-face))
              collect (match-string-no-properties 1)))))

(provide 'anonymize)
