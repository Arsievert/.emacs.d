;;; languages.el    -*- lexical-binding: t -*-

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

;; Language/LSP specific package installation and settings.

;;; Code:

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pylsp"))))

;; C packages and configurations
(use-package cc-mode
  :ensure nil
  :custom
  (tab-width 4)
  (c-basic-offset 4)
  (c-set-style "linux")
  (indent-tabs-mode nil)

  :config
  ;; C function highlighting.
  (font-lock-add-keywords 'c-mode
                          '(("\\<\\([a-z0-9A-Z_]*\\)(" 1 font-lock-function-name-face))))

;; Rust packages and configurations
(use-package rust-mode
  :ensure t)

;; Fish shell language
(use-package fish-mode
  :ensure t)

(use-package pyvenv
  :ensure t
  :hook (python-mode . pyvenv-mode)
  :config
  ;; This function automatically activates a .venv directory in the current project root.
  ;; Usage: M-x activate-venv
  ;;        Run from anywhere in your project to activate the .venv
  (defun activate-venv ()
    "Activate .venv in the current project root"
    (interactive)
    ;; Configure directory to root of project
    (let ((default-directory (locate-dominating-file "." ".git")))
      (if default-directory
          (let ((venv (expand-file-name ".venv" default-directory)))
            (if (file-directory-p venv)
                (pyvenv-activate venv)))))))

(use-package dape
  :ensure t)

(use-package copilot
  :ensure t
  ;;:hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  :config
  ;; Node.js version 18+ required
  (setq copilot-node-executable "/usr/bin/node") ; Adjust path as needed
  (setq copilot-idle-delay 0.5) ; Delay before showing completions
  (setq copilot-indent-offset-warning-disable t))

(use-package dockerfile-mode
  :ensure t)
