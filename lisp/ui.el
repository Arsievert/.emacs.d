;;; ui.el    -*- lexical-binding: t -*-

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

;; Installs and configures packages used for UI.

;;; Code:

;; Remove unecessary window elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setopt inhibit-splash-screen 't)

(use-package nerd-icons-completion
  :ensure t
  :when (or (display-graphic-p) (daemonp))
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package doom-themes
  :ensure t
  :init
  (setopt doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (if (or (display-graphic-p) (daemonp))
      ;; Use gui-theme for GUI.
      (load-theme gui-theme t)
      ;; Use terminal-theme for terminal.
      (load-theme terminal-theme t)))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode)
  (unless after-init-time
    (setopt doom-modeline--old-format mode-line-format)
    (setopt-default mode-line-format nil))
  ;; Properties
  (setopt doom-modeline-major-mode-color-icon t)
  (setopt doom-modeline-buffer-modification-icon t)
  (setopt doom-modeline-minor-modes nil)
  (setopt doom-modeline-enable-word-count nil)
  (setopt doom-modeline-buffer-encoding t)
  (setopt doom-modeline-indent-info nil)
  (setopt doom-modeline-checker-simple-format t)
  (setopt doom-modeline-vcs-max-length 30)
  (setopt doom-modeline-persp-name t)
  (setopt doom-modeline-persp-name-icon t)
  (setopt doom-modeline-lsp t)
  (setopt doom-modeline-github nil)
  (setopt doom-modeline-env-version t)
  (setopt doom-modeline-buffer-file-name-style 'file-name)
  (setopt doom-modeline-before-update-env-hook nil)
  (setopt doom-modeline-after-update-env-hook nil)
  ;; Don't use icons in terminal mode
  :custom
  (when (or (display-graphic-p) (daemonp))
    (doom-modeline-icon t)
    (doom-modeline-major-mode-icon t)))

(use-package ligature
  :ensure t
  :config
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
    '("==" "===" "====" "=>" "=|" "=>|" "==>" "===>" "=/=" "!=="
      "!=" "!==" "!!" "!!!" "!." "!:" "!!." "!~"
      "--" "---" "----" "-<" "->" "->>" "-|" "-~" "-*-" "-->"
      "++" "+++" "++++" "+>" "+:" "+-" "-+"
      "::" ":::" "::::" ":=" ":>" ":<" ":>:" ":<:" ":+" ":-" ":/" ":\\" ":|" ":||"
      "//" "///" "////" "/*" "*/" "/>" "/<" "/=" "//=" "/==" "///=" "/**"
      "<<" "<<<" "<<<<" "<<=" "<~" "<~~" "<~>" "<*" "<*>" "<|" "<|>" "<$" "<$>" "<+" "<+>"
      "</" "</>" "<//" "<>" "<!--" "<--" "<-" "<->" "<=" "<==" "<=>" "<<="
      ">>" ">>>" ">>>>" ">>=" ">>-" ">-" ">=>" ">=" ">==" ">>==" ">>>=" ">>="
      "||" "|||" "||=" "|=" "|>" "|->" "|=>" "|<" "|<>" "|-" "|--" "|---" "|----"
      ".." "..." "...." ".~" ".=" ".-" ".?" "..=" "..<"
      "**" "***" "****" "*/" "*>" "*/" "*/>" "*/>"
      "\\\\" "\\\\\\" "\\/" "\\n" "\\t"
      "~~" "~~~" "~=" "~-" "~@" "~>" "~~>"
      "^=" "^--" "^->" "^=>" "^<<" "^>>"
      "&&" "&&&" "&=" "&+" "&-" "&/" "&*"
      "%%" "%%%" "%>" "%<" "%=" "%/"
      "##" "###" "####" "#{" "#[" "#(" "#?" "#_" "#:" "#=" "#!" "#/"
      "@@" "@@@" "@>" "@<" "@~" "@*" "@/"
      "$$" "$$$" "$>" "$<"
      "??" "???" "?." "?:"
      ";;" ";;;"
      "__" "___" "____" "_<" "_>" "_|" "_~" "_-" "_+"
      "www" "wwww"
      "<=>" "<!--" "-->" "</>" "<//" "/>" "/>"
      "0x" "0X" "0b" "0B"
      "x:"))

  ;; Enable ligatures in specific modes
  (ligature-set-ligatures 'emacs-lisp-mode
    '(";;" ";;;" ";;!!" ";;" "!!" "!=" "!==" "#(" "#_(" "#{" "#[" "#_" "#_(" "#?" "#\\"))

  ;; Python-specific ligatures
  (ligature-set-ligatures 'python-mode
    '("->" "=>" "!=" "==" "<=" ">=" ":=" "//" "///" "/*" "*/" "**" "***" "..." "__"))

  ;; Rust-specific ligatures
  (ligature-set-ligatures 'rust-mode
    '("->" "=>" "::" ":::" ".." "..=" "..." "!=" "==" "<=" ">=" "&&" "||" "<<" ">>"
      "|>" "<|" "??" "/*" "*/" "//" "///" "//!" "/**" "**/"))

  ;; C/C++ ligatures
  (ligature-set-ligatures '(c-mode c++-mode)
    '("->" "<<" ">>" "==" "!=" "<=" ">=" "&&" "||" "::" "++'" "--" "/*" "*/" "//" "///"
      "/**" "**/"))

  ;; Markdown ligatures
  (ligature-set-ligatures 'markdown-mode
    '("---" "===" "```" "~~~" "..." "<<" ">>" "->" "=>" "<-" "<!--" "-->"))

  ;; Org mode ligatures
  (ligature-set-ligatures 'org-mode
    '("->" "=>" "<-" "..." "<<" ">>" "<<<" ">>>" "::" ":::" "#+"))

  ;; Global ligatures (apply to all modes)
  ;; Be conservative with global ligatures to avoid conflicts
  (when ligatures
  (global-ligature-mode t)))
