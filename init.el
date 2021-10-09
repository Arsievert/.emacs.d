;;; init.el    -*- lexical-binding: t -*-

;; Copyright (C) 2021 Austin Sievert

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

;; This file should only be used to load modules which
;; do all the configuration, customization, and package management.

;; The order in which modules are loaded is very important.

;;; Code:

;; Load Configurations
(load (concat user-emacs-directory "config"))

;; Load modules
(load (concat user-emacs-directory "custom"))
(load (concat user-emacs-directory "lisp/packages"))
(load (concat user-emacs-directory "lisp/ui"))
(load (concat user-emacs-directory "lisp/behavior"))
(load (concat user-emacs-directory "lisp/qol"))

;; Restore garbage collection to sane value.
(setq gc-cons-threshold 16777216) ;16mb
