;;; ui.el    -*- lexical-binding: t -*-

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

;; Installs and configures packages used for UI.

;;; Code:

(use-package all-the-icons)

(use-package ivy-rich
  :init
  (ivy-rich-mode t))

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

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  :bind
  (:map dashboard-mode-map
        ("<down-mouse-1>" . nil)
        ("<mouse-1>" . widget-button-click)
        ("<mouse-2>" . widget-button-click))
  :config
  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-banner-logo-title "Emacs")
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)))
  (setq dashboard-set-navigator t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-navigator-buttons
        `(((,(when (display-graphic-p)
               (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
            "Homepage"
            "Visit Emacs Home"
            (lambda (&rest _) (browse-url "https://www.gnu.org/software/emacs/")))
           (,(when (display-graphic-p)
               (all-the-icons-octicon "archive" :height 1.1 :v-adjust 0.0))
            "Update Emacs"
            "Update Emacs"
            (lambda (&rest _) (update-emacs)))
           (,(when (display-graphic-p)
               (all-the-icons-faicon "repo-pull" :height 1.1 :v-adjust 0.0))
            "Update Packages"
            "Update Packages"
            (lambda (&rest _) (straight-normalize-all)))))))
