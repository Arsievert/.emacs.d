;;; bb-mode.el    -*- lexical-binding: t -*-

;; Copyright (C) 2026 Austin Sievert

;; Author: Austin Sievert <Arsievert1@gmail.com>
;; URL: https://github.com/Arsievert/.emacs.d

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A small, self-contained BitBake major mode: font-lock, comment/string
;; syntax, and imenu.  No task running, no polymode, no dependencies.
;; Named `bb-mode' (not `bitbake-mode') so it can coexist with the MELPA
;; `bitbake' package.

;;; Code:

(defvar bb-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?. "_" table)
    table)
  "Syntax table for `bb-mode'.")

(defconst bb-font-lock-keywords
  `(;; Assignments: [export] VAR[:override...][[flag]] = / ?= / ??= / := / += / =+ / .= / =.
    (,(concat "^\\(?:export[ \t]+\\)?"
              "\\([[:alnum:]_~][[:alnum:]_~+.${}/-]*\\)"      ; 1: variable
              "\\(\\(?::[[:alnum:]_${}+.-]+\\)*\\)"           ; 2: overrides
              "\\(?:\\[\\([[:alnum:]_+.-]+\\)\\]\\)?"         ; 3: varflag
              "[ \t]*\\(?:=\\+\\|\\+=\\|\\?\\?=\\|\\?=\\|:=\\|\\.=\\|=\\.\\|=\\)")
     (1 font-lock-variable-name-face)
     (2 font-lock-type-face)
     (3 font-lock-constant-face nil t))
    ;; :append / :prepend / :remove stand out from machine/distro overrides
    (":\\(append\\|prepend\\|remove\\)\\_>"
     (1 font-lock-builtin-face t))
    ;; Task/function definitions: [fakeroot] [python] name() {
    ("^\\(?:fakeroot[ \t]+\\)?\\(?:python[ \t]+\\)?\\([[:alnum:]_${}:.+-]+\\)[ \t]*([ \t]*)[ \t]*{"
     (1 font-lock-function-name-face))
    ;; Inline python defs
    ("^[ \t]*def[ \t]+\\([[:alnum:]_]+\\)"
     (1 font-lock-function-name-face))
    ;; Directive keywords
    (,(concat "^[ \t]*"
              (regexp-opt '("inherit" "inherit_defer" "require" "include"
                            "include_all" "export" "unset" "addtask" "deltask"
                            "addhandler" "addpylib" "addfragments"
                            "EXPORT_FUNCTIONS" "INHERIT" "fakeroot" "python"
                            "def")
                          'symbols))
     (1 font-lock-keyword-face))
    ("\\_<\\(?:before\\|after\\)\\_>" . font-lock-keyword-face)
    ;; ${@inline python} and ${VAR} expansions — also inside strings
    ("\\${@[^}\n]*}" 0 font-lock-preprocessor-face t)
    ("\\${[^@}\n][^}\n]*}" 0 font-lock-variable-name-face t)
    ;; Task name references (addtask lines, dependencies, etc.)
    ("\\_<do_[[:alnum:]_]+\\_>" 0 font-lock-function-name-face))
  "Font-lock rules for `bb-mode'.")

(defvar bb-imenu-generic-expression
  '(("Tasks" "^\\(?:fakeroot[ \t]+\\)?\\(?:python[ \t]+\\)?\\(do_[[:alnum:]_]+\\)[ \t]*(" 1)
    ("Defs" "^[ \t]*def[ \t]+\\([[:alnum:]_]+\\)" 1))
  "Imenu patterns for `bb-mode'.")

;;;###autoload
(define-derived-mode bb-mode prog-mode "BitBake"
  "Major mode for editing BitBake recipes, classes, appends and includes."
  :syntax-table bb-mode-syntax-table
  (setq-local comment-start "#"
              comment-start-skip "#+[ \t]*"
              comment-end ""
              font-lock-defaults '(bb-font-lock-keywords)
              imenu-generic-expression bb-imenu-generic-expression))

(provide 'bb-mode)
;;; bb-mode.el ends here
