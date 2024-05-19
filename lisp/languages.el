;;; languages.el    -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Austin Sievert

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

;; Language/LSP specific package installation and settings.

;;; Code:

;; C packages and configurations
(use-package cc-mode
  :ensure nil
  :hook
  (c-mode-common . (lambda ()
                     (c-set-style "linux")
                     (setq tab-width 4)
                     (setq c-basic-offset 4)
                     (setq indent-tabs-mode nil)))
  :config
  ;; C function highlighting.
  (font-lock-add-keywords 'c-mode
                          '(("\\<\\([a-z0-9A-Z_]*\\)(" 1 font-lock-function-name-face))))

;; Rust packages and configurations
(use-package rust-mode
  :ensure t)
