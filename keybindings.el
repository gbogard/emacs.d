(require 'bind-key)

(sp-use-paredit-bindings)
(bind-key* "C-d" 'mc/mark-next-like-this)
(bind-key* "C-c i" 'indent-region)
(bind-key* "C-c SPC" 'ace-jump-mode)
(bind-key* "C-c C-p p" 'projectile-switch-project)
(global-set-key [f8] 'neotree-project-dir)
(global-set-key [f7] 'neotree-refresh)
(bind-key* "C-c f" 'ag)

;; Move windows with ctrl
(windmove-default-keybindings 'C)
