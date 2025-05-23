;;; config.el    -*- lexical-binding: t -*-

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

;; Settings provided to quickly change the behavior and look of Emacs.

;;; Code:

;; Terminal-Theme:
;; Selected color scheme using Terminal Emacs.
(setopt terminal-theme 'doom-gruvbox)

;; Terminal-Use-Theme-Background:
;; 't - use background color as specified by selected terminal theme.
;; 'nil - use background color as specified by terminal window.
(setopt terminal-use-theme-background 'nil)

;; GUI-Theme:
;; Selected color scheme when using GUI Emacs.
(setopt gui-theme 'doom-gruvbox)

;; Font:
;; Faces used when using GUI Emacs.
(setopt font "Fantasque Sans Mono")

;; Font-Height:
;; Font size used when using GUI Emacs.
(setopt font-height 150)

;; Terminal-Mouse
;; 1 - enable xterm mouse support.
;; 0 - disable xterm mouse support.
(setopt terminal-mouse 0)

;; Whitespace-Cleanup:
;; 't - remove trailing whitespace automatically.
;; 'nil - do not remove trailing whitespace.
(setopt whitespace-cleanup 't)

;; Custom Modules
;; Directory or file which contains additional elisp configurations.
;; Wildcard can be used to load multiple files within a directory.
(setopt custom-modules "")
