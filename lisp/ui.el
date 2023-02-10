;;; ui.el    -*- lexical-binding: t -*-

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

;; Installs and configures packages used for UI.

;;; Code:

;; Remove unecessary window elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-splash-screen 't)

(use-package all-the-icons)

(use-package ivy-rich
  :init
  (ivy-rich-mode t)
  ;; Patch for bug related to "Not a Directory".
  ;; Author: BlindingDark
  ;; URL: https://github.com/Yevgnen/ivy-rich/issues/115
  (defun ivy-rich--switch-buffer-directory! (orig-fun &rest args)
         (cl-letf (((symbol-function 'directory-file-name) #'file-name-directory))
           (apply orig-fun args)))
  (advice-add 'ivy-rich--switch-buffer-directory :around #'ivy-rich--switch-buffer-directory!))

(use-package all-the-icons-ivy-rich
  :init
  (all-the-icons-ivy-rich-mode t))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (if (or (display-graphic-p) (daemonp))
      ;; Use gui-theme for GUI.
      (load-theme gui-theme t)
      ;; Use terminal-theme for terminal.
      (load-theme terminal-theme t)))

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :init
  (unless after-init-time
    (setq doom-modeline--old-format mode-line-format)
    (setq-default mode-line-format nil))
  ;; Properties
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-vcs-max-length 30)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-persp-name-icon t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil)
  ;; Don't use icons in terminal mode
  :config
  (when (or (display-graphic-p) (daemonp))
    (setq doom-modeline-icon t)
    (setq doom-modeline-major-mode-icon t)))
