;;; packages.el    -*- lexical-binding: t -*-

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

;; General package installation and settings.

;;; Code:

;; Bootstrap Straight.el
;; Author: Radon Rosborough
;; URL: https://github.com/raxod502/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package org
  :config
  ;; Allow execution of code snippets.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C      . t)
     (lua    . t)
     (python . t))))

(use-package magit
  :config
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq ediff-split-window-function 'split-window-horizontally)
  :bind (("C-x g" . magit-status)))

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq ivy-height 7)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :bind
  (;; standard keybinds
   ("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ;; system keybinds
   ("C-c c" . counsel-compile)
   ("C-c g" . counsel-git)      ;;project wide file finder
   ("C-c j" . counsel-git-grep) ;;project wide grep
   ("C-c k" . counsel-ag)
   ("C-c f" . counsel-fzf)
   ("C-x l" . counsel-locate))
  :config
  ;; Tramp ivy interface
  (use-package counsel-tramp
    :bind (("C-c t" . counsel-tramp))))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (add-to-list 'company-backends 'company-etags))

(use-package evil
  :init
  (evil-mode 1))

(use-package which-key
  :init
  (which-key-mode 1))

(use-package cc-mode
  :ensure nil
  :hook
  (c-mode-common . (lambda ()
                     (c-set-style "linux")
                     (setq tab-width 4)
                     (setq c-basic-offset 4)))
  :config
  ;; C function highlighting.
  (font-lock-add-keywords 'c-mode
                          '(("\\<\\([a-z0-9A-Z_]*\\)(" 1 font-lock-function-name-face))))

(use-package xcscope
  :init
  (cscope-setup))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :bind
  (;; Rebind important yasnippet actions
   ("C-c y i" . yas-insert-snippet)
   ("C-c y n" . yas-new-snippet)
   ("C-c y v" . yas-visit-snippet-file)))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

(use-package server
  :ensure nil
  :if window-system
  :hook (after-init . server-mode))

(use-package htmlize)

(when (not (or (display-graphic-p) (daemonp)))
  (semantic-mode 1)
  (global-semantic-stickyfunc-mode 1))
