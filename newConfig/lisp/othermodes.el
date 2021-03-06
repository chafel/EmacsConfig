(use-package autoinsert
  :init
  (add-hook 'python-mode-hook 'auto-insert)
  :config
  (setq auto-insert-query nil
        auto-insert-directory "~/.emacs.d/templates/")
  (define-auto-insert "\.py" "python.py"))


(use-package desktop
  :init
  (desktop-save-mode 1)
  :config
  (setq desktop-restore-eager 20
        desktop-lazy-verbose nil
        desktop-base-file-name "desktop"
        desktop-base-lock-name "desktop.lock")
  (add-to-list 'desktop-modes-not-to-save '(dired-mode fundamental-mode)))


(use-package eldoc
  :diminish eldoc-mode)


(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))


(use-package ido
  :init (ido-mode)
  :config
  (setq ido-show-dot-for-dired t
        ido-enable-flex-matching nil))


(use-package linum
  :init (add-hook 'prog-mode-hook 'linum-mode)
  :commands (linum-mode))


(use-package newcomment
  :bind (("C-'" . comment-dwim)
         ("C-c g" . comment-or-uncomment-region)))


(use-package wdired
  :init
  (add-hook 'dired-mode-hook
            '(lambda ()
               (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode))))


(use-package whitespace
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (add-hook 'prog-mode-hook
            '(lambda () (setq show-trailing-whitespace t)))
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face lines-tail))
  :diminish whitespace-mode)


(use-package ag
  :bind (("C-c C-s" . ag-project))
  :config (setq ag-reuse-window t
                ag-highlight-search t))


(use-package avy
  :bind ("C-," . avy-goto-char-2)
  :ensure t)


(use-package diff-hl
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :ensure t)


(use-package editorconfig
  :ensure t)


(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :ensure t)


(use-package gitattributes-mode
  :ensure t)


(use-package gitconfig-mode
  :ensure t)


(use-package gitignore-mode
  :ensure t)


(use-package helm
  :bind (("C-x b" . helm-mini)
         ("M-x" . helm-M-x)
         ("C-c h" . helm-projectile)
         ("C-c s" . helm-projectile-ag)
         ("C-x C-y" . helm-show-kill-ring))
  :ensure t
  :config
  (use-package helm-ag
    :ensure t)
  (use-package helm-config)
  (use-package helm-command
    :config (setq helm-M-x-fuzzy-match t))
  (use-package helm-projectile
    :ensure t))


(use-package ibuffer-vc
  :ensure t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))


(use-package indium
  :ensure t)

(use-package rjsx-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :config
  (use-package lsp-ui
    :init (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    :ensure t)
  (use-package lsp-php
    :init (add-hook 'php-mode-hook #'lsp-php-enable)
    :ensure t)
  (use-package lsp-javascript-typescript
    :init
    (add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
    (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable)
    :ensure t))


(use-package magit
  :bind ("C-x C-z" . magit-status)
  :ensure t
  :config
  (use-package magit-imerge
    :ensure t))


(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :ensure t)


(use-package osx-dictionary
  :if (eq system-type 'darwin)
  :bind (("C-c c" . osx-dictionary-search-input)
         ("C-c d" . osx-dictionary-search-pointer))
  :ensure t)


(use-package projectile
  :ensure t
  :config (projectile-mode)
  :diminish projectile-mode)


(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :ensure t
  :commands rainbow-delimiters-mode)


(use-package rainbow-identifiers
  :init (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
  :ensure t
  :commands rainbow-identifiers-mode)


(use-package restart-emacs
  :ensure t)


(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (use-package smartparens-config)
  :diminish smartparens-mode)


(use-package smex
  :bind ("C-x <RET>" . smex)
  :ensure t)


(use-package string-edit
  :bind ("C-c i" . string-edit-at-point)
  :ensure t)


(use-package switch-window
  :bind ("C-x o" . switch-window)
  :ensure t)


(use-package tagedit
  :init (dolist (hook '(sgml-mode-hook))
          (add-hook hook #'tagedit-mode))
  :ensure t)


(use-package tern
  :init (dolist (hook '(js2-mode-hook web-mode-hook))
          (add-hook hook #'tern-mode))
  :ensure t
  :diminish tern-mode)


;;(use-package tide
;;  :init (dolist (hook '(js-mode-hook typescript-mode-hook))
;;          (add-hook hook #'tide-setup))
;;  :ensure t
;;  :config (progn
;;            (tide-hl-identifier-mode +1)
;;            (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)))
;;:config (tagedit-add-paredit-like-keybindings)
;;(tagedit-add-experimental-features))


(use-package wgrep-ag
  :init (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  :ensure t)


(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(provide 'othermodes)
