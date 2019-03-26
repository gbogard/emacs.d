;; JS support
(use-package js2-mode
  :ensure t
  :init
  (setq js-basic-indent 2)
  (setq js2-indent-swith-body t)
  (advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))
  (setq-default js2-basic-indent 2
                js2-indent-switch-body t
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespaces t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t))

;; Typescript support
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :defer 2
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'js-mode-hook #'setup-tide-mode)
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  (add-hook 'rjsx-mode-hook #'setup-tide-mode))

(defun my/use-eslint-from-node-modules ()
  "Use the local version of eslint if any."
  (let ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               (lambda (dir)
                 (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" dir)))
                   (and eslint (file-executable-p eslint)))))))
    (when root
      (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
        (setq-local flycheck-javascript-eslint-executable eslint)))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; React support
(use-package rjsx-mode
  :ensure t)

;; CSS support
(use-package css-mode
  :ensure t
  :defer 5
  :config
  (setq css-indent-offset 2))

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :custom
  (scss-compile-at-save nil))

;; Scala support
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package ensime
  :ensure t
  :pin melpa-stable)

;; Clojure support
(use-package cider-eval-sexp-fu
  :defer t)

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode)
  (sp-use-paredit-bindings))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t                  
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t    
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t            
        cider-overlays-use-font-lock t)         
  (cider-repl-toggle-pretty-printing))

(setq ensime-startup-notification nil)

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
