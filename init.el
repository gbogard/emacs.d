;;; .emacs --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa" . 1)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose nil)
(eval-when-compile
  (require 'use-package))

(load "~/.emacs.d/theme.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/autocomplete.el")
(load "~/.emacs.d/languages.el")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(delete-selection-mode)

;; Required on Macos
(use-package exec-path-from-shell
  :ensure t
  :config
  (if (eq system-type 'darwin)
      (exec-path-from-shell-initialize)))

;; WSL Support on Windows
(if (eq system-type 'windows-nt)
    (setq explicit-shell-file-name "C:/Windows/System32/bash.exe"))

;; Editor config
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Project management
(use-package ivy
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  (setq projectile-indexing-method 'alien)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

;; Tree view
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;; Sidebar
(use-package neotree
  :ensure t
  :config
  (setq neo-autorefresh nil)
  (setq neo-window-fixed-size nil)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq-default neo-show-hidden-files t))

;; smex
(use-package smex
  :ensure t
  :bind
  (([remap execute-extended-command] . smex)
   ("M-X" . smex-major-mode-commands)))

;; Quick navigation
(use-package ace-jump-mode
  :ensure t)

;; Get backup files out of the way
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;; Magit
(use-package magit
  :ensure t)

 ;; Multiple cursors
(use-package multiple-cursors
  :ensure t)

;; Search
(use-package ag
  :ensure t)

;; Narrow search using x and m
(use-package winnow
  :ensure t
  :config
  (add-hook 'ag-mode-hook 'winnow-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; Clean buffers automatically
(setq clean-buffer-list-delay-general (* 60 30))
(run-with-timer 0 (* 30 60) 'clean-buffer-list)
