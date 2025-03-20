;;; qol.el    -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Austin Sievert

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

;; Presents functions commonly used for development tasks.

;;; Code:

(defun refresh-buffer()
  "Revert buffer without confirmation"
  (interactive)
  (revert-buffer :ignore :noconfirm))

(defun dos2unix ()
  "Swap line endings (dos->unix)"
  (interactive)
  (set-buffer-file-coding-system 'unix)
  (save-buffer))

(defun unix2dos ()
  "Swap line endings (unix->dos)"
  (interactive)
  (set-buffer-file-coding-system 'dos)
  (save-buffer))

(defun find-tags ()
  "Find tags table for current git repository"
  (interactive)
  ;; Configure directory to root of project
  (let ((default-directory(locate-dominating-file "." ".git")))
    (visit-tags-table "TAGS")))

(defun ets ()
  "Create etags and cscope files"
  (interactive)
  ;; Configure directory to root of project
  (let ((default-directory(locate-dominating-file "." ".git")))
    (with-current-buffer (get-buffer-create "*output*") (erase-buffer))
    (call-process-shell-command "find . -type f -iname \"*.[ch]\" | etags -;")
    (find-tags)))

(defun update-emacs ()
  "Recompile emacs from latest source code"
  (interactive)
  (with-current-buffer (get-buffer-create "*update*") (erase-buffer))
  (async-shell-command "cd ~/;
                        ~/.emacs.d/sh/build-emacs;"))
