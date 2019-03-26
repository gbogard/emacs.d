;; Theme
(use-package all-the-icons
  :ensure t)

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Modline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  ;; Enable line numbers
  (global-display-line-numbers-mode t)
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
  (column-number-mode))

;; Disable GUI and toolbars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq comint-prompt-read-only t)

;;; no tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(setq js-indent-level 2)

;; Highlight Indent guide
(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

;; Display emojis
(use-package emojify
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))

;; Set font size on Macos
(set-default-font "Menlo 14")

;; Nyan modline
(use-package nyan-mode
  :init (nyan-mode t)
  :config (progn
            (setq nyan-bar-length 48)
            (setq nyan-cat-face-number 4))
  :ensure t)
