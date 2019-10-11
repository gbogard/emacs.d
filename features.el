;;  Features

;; Dashboard
(straight-use-package 'dashboard)
(require 'dashboard)
(dashboard-setup-startup-hook)

;; Tree view
(straight-use-package 'neotree)
(require 'neotree)

;; Project management
(straight-use-package 'ivy)
(straight-use-package 'projectile)
(require 'projectile)
(setq projectile-completion-system 'ivy)
(projectile-mode +1)

;; Indent guides
(straight-use-package 'highlight-indent-guides)
(setq highlight-indent-guides-method 'fill)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; Parens grouping
(straight-use-package 'smartparens)
(add-hook 'prog-mode-hook #'smartparens-strict-mode)
