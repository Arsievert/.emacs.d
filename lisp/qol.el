;;; qol.el    -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Austin Sievert

;; Author: Austin Sievert <Arsievert1@gmail.com>
;; URL: https://github.com/Arsievert1/.emacs.d

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

(defun reload-config()
  "Reload emacs configuration"
  (interactive)
  (load-file user-init-file)
  (mapc 'load-file (file-expand-wildcards "~/.emacs.d/lisp/*")))

(defun refresh-buffer()
  "Revert buffer without confirmation"
  (interactive)
  (revert-buffer :ignore :noconfirm))

(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun save-all-buffers ()
  "Save all buffers."
  (interactive)
  (mapc 'save-buffer (buffer-list)))

(defun dos2unix ()
  "Swap line endings (dos->unix)"
  (interactive)
  (set-buffer-file-coding-system 'unix)
  (save-buffer))

(defun dos2unix_project ()
  "Swap line endings (dos->unix) for an entire project."
  (interactive)
  ;; Configure directory to root of project
  (let ((default-directory(locate-dominating-file "." ".git")))
    (shell-command "find . -type f -iname \"*.[ch]\" -print0 | xargs -0 dos2unix --")))

(defun unix2dos ()
  "Swap line endings (unix->dos)"
  (interactive)
  (set-buffer-file-coding-system 'dos)
  (save-buffer))

(defun unix2dos_project ()
  "Swap line endings (unix->dos) for an entire project."
  (interactive)
  ;; Configure directory to root of project
  (let ((default-directory(locate-dominating-file "." ".git")))
    (shell-command "find . -type f -iname \"*.[ch]\" -print0 | xargs -0 unix2dos --")))

(defun find-tags ()
  "Find tags table for current git repository"
  (interactive)
  ;; Configure directory to root of project
  (let ((default-directory(locate-dominating-file "." ".git")))
    (let ((tags-revert-without-query t))
      (visit-tags-table "TAGS"))))

(defun ets ()
  "Create etags and cscope files"
  (interactive)
  ;; Configure directory to root of project
  (let ((default-directory(locate-dominating-file "." ".git")))
    (with-current-buffer (get-buffer-create "*output*") (erase-buffer))
    (call-process-shell-command "find . -type f -iname \"*.[ch]\" | etags -;
                                 cscope -Rbquv;
                                 find . -type f -iname \"*.[ch]\" > cscope.files;")
    (find-tags)))

(defun update-emacs ()
  "Recompile emacs from latest source code"
  (interactive)
  (with-current-buffer (get-buffer-create "*update*") (erase-buffer))
  (async-shell-command "cd ~/;
                        ~/.emacs.d/sh/build-emacs;"))
