;;; config.el    -*- lexical-binding: t -*-

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

;; Settings provided to quickly change the behavior and look of Emacs.

;;; Code:

;; Terminal-Theme:
;; Selected color scheme using Terminal Emacs.
(setq terminal-theme 'modus-vivendi)

;; Terminal-Use-Theme-Background:
;; 't - use background color as specified by selected terminal theme.
;; 'nil - use background color as specified by terminal window.
(setq terminal-use-theme-background 'nil)

;; GUI-Theme:
;; Selected color scheme when using GUI Emacs.
(setq gui-theme 'doom-solarized-dark)

;; Font:
;; Faces used when using GUI Emacs.
(setq font "Fantasque Sans Mono")

;; Terminal-Mouse
;; 1 - enable xterm mouse support.
;; 0 - disable xterm mouse support.
(setq terminal-mouse 0)

;; Whitespace-Cleanup:
;; 't - remove trailing whitespace automatically.
;; 'nil - do not remove trailing whitespace.
(setq whitespace-cleanup 't)
