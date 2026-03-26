;;; config.el    -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Austin Sievert

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

;; To override defaults, add a setopt after the defcustom block:
;; (setopt my/terminal-mouse 1)

;;; Code:

(defgroup my/config nil
  "Personal Emacs configuration."
  :group 'convenience)

;; Terminal-Theme:
;; Selected color scheme using Terminal Emacs.
(defcustom my/terminal-theme 'doom-gruvbox
  "Color scheme for terminal Emacs."
  :type 'symbol
  :group 'my/config)

;; Terminal-Use-Theme-Background:
;; t - use background color as specified by selected terminal theme.
;; nil - use background color as specified by terminal window.
(defcustom my/terminal-use-theme-background nil
  "Use background color from terminal theme instead of terminal window."
  :type 'boolean
  :group 'my/config)

;; GUI-Theme:
;; Selected color scheme when using GUI Emacs.
(defcustom my/gui-theme 'doom-gruvbox
  "Color scheme for GUI Emacs."
  :type 'symbol
  :group 'my/config)

;; Font:
;; Faces used when using GUI Emacs.
(defcustom my/font "Berkeley Mono"
  "Font family for GUI Emacs."
  :type 'string
  :group 'my/config)

;; Font options:
;; Font size and weight used when using GUI Emacs.
(defcustom my/font-height 160
  "Font height for GUI Emacs."
  :type 'integer
  :group 'my/config)

(defcustom my/font-weight 'light
  "Font weight for GUI Emacs."
  :type 'symbol
  :group 'my/config)

;; Ligatures:
;; Enable or disable ligatures - requires ligature support in selected font
(defcustom my/ligatures t
  "Enable font ligatures."
  :type 'boolean
  :group 'my/config)

;; Terminal-Mouse
;; 1 - enable xterm mouse support.
;; 0 - disable xterm mouse support.
(defcustom my/terminal-mouse 0
  "Enable xterm mouse support (1 = enabled, 0 = disabled)."
  :type 'integer
  :group 'my/config)

(setopt my/terminal-mouse 1)

;; Whitespace-Cleanup:
;; t - remove trailing whitespace automatically.
;; nil - do not remove trailing whitespace.
(defcustom my/whitespace-cleanup t
  "Remove trailing whitespace automatically on save."
  :type 'boolean
  :group 'my/config)

;; Custom Modules
;; Directory or file which contains additional elisp configurations.
;; Wildcard can be used to load multiple files within a directory.
(defcustom my/custom-modules ""
  "Additional elisp configuration files to load (supports wildcards)."
  :type 'string
  :group 'my/config)
