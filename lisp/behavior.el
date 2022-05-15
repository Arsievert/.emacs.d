;;; behavior.el    -*- lexical-binding: t -*-

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

;; Controls settings which affect the overall behavior of emacs.

;;; Code:

;; Ensure proper background color is used.
(defun terminal-config ()
  (unless (or (display-graphic-p) (daemonp) terminal-use-theme-background)
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'terminal-config)

(defun new-frame-config (frame)
  (terminal-config)
  (with-selected-frame (or frame (selected-frame))
    (set-frame-size (selected-frame) 240 70)))

(add-hook 'after-make-frame-functions 'new-frame-config)

(setq x-select-enable-clipboard t)
(setq x-underline-at-descent-line t)

;; Terminal mouse support.
(xterm-mouse-mode terminal-mouse)

;; Smooth Scrolling
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; Typing over a highlighted region
;; deletes what was previously there.
(delete-selection-mode t)

;; Disable backup files.
(setq make-backup-files nil)

;; Prevent tabs at the beginning of lines
(setq-default indent-tabs-mode nil)

;;Trailing Whitepsace
(setq-default show-trailing-whitespace nil)
(if whitespace-cleanup
    (add-hook 'before-save-hook 'delete-trailing-whitespace))
