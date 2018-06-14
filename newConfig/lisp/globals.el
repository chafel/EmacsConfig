
(prefer-coding-system 'utf-8)


(setq fill-column 80)


(setq frame-title-format "%b @ %f")


(defalias 'yes-or-no-p 'y-or-n-p)


(use-package autorevert
  :init (global-auto-revert-mode 1)
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  :diminish auto-revert-mode)


(use-package better-defaults
  :ensure t)


(use-package delsel
  :init (delete-selection-mode t))


(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (dolist (var '("GOPATH" "PYTHONPATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))


(use-package hl-line
  :init (global-hl-line-mode))


(use-package no-littering
  :ensure t)

(use-package auto-complete
  :ensure t)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(defun auto-complete-mode-maybe ()
  "No maybe for you. Only AC!"
  (auto-complete-mode 1))
(ac-set-trigger-key "TAB")
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

(use-package smartparens
  :ensure t)
(require 'smartparens-config)
(add-hook 'js-mode-hook #'smartparens-mode)
;; (use-package smartparens-config
;;     :ensure smartparens
;;     :config
;;     (progn
;;       (show-smartparens-global-mode t)))
(setq show-smartparens-global-mode t)
(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
(require 'bind-key)
(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)
 
 ("C-<down>" . sp-down-sexp)
 ("C-<up>"   . sp-up-sexp)
 ("M-<down>" . sp-backward-down-sexp)
 ("M-<up>"   . sp-backward-up-sexp)
 
 ("C-M-f" . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)
 
 ("C-M-n" . sp-next-sexp)
 ("C-M-p" . sp-previous-sexp)
 
 ("C-S-f" . sp-forward-symbol)
 ("C-S-b" . sp-backward-symbol)
 
 ("C-<right>" . sp-forward-slurp-sexp)
 ("M-<right>" . sp-forward-barf-sexp)
 ("C-<left>"  . sp-backward-slurp-sexp)
 ("M-<left>"  . sp-backward-barf-sexp)
 
 ("C-M-t" . sp-transpose-sexp)
 ("C-M-k" . sp-kill-sexp)
 ("C-k"   . sp-kill-hybrid-sexp)
 ("M-k"   . sp-backward-kill-sexp)
 ("C-M-w" . sp-copy-sexp)
 
 ("C-M-d" . delete-sexp)
 
 ("M-<backspace>" . backward-kill-word)
 ("C-<backspace>" . sp-backward-kill-word)
 ([remap sp-backward-kill-word] . backward-kill-word)
 
 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp)
 
 ("C-x C-t" . sp-transpose-hybrid-sexp)
 
 ("C-c ("  . wrap-with-parens)
 ("C-c ["  . wrap-with-brackets)
 ("C-c {"  . wrap-with-braces)
 ("C-c '"  . wrap-with-single-quotes)
 ("C-c \"" . wrap-with-double-quotes)
 ("C-c _"  . wrap-with-underscores)
 ("C-c `" . wrap-with-back-quotes))

(use-package server
  :init (server-start))

(use-package neotree
  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :ensure t)
(use-package all-the-icons
  :ensure t)

(when (string-match "apple-darwin" system-configuration)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        ns-function-modifier 'hyper
        mac-allow-anti-aliasing t)
  (when (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode)))


(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

(use-package auto-indent-mode
  :ensure t)
;;(setq auto-indent-on-visit-file t) ;; If you want auto-indent on for files
(require 'auto-indent-mode)
(auto-indent-global-mode)

(autoload 'auto-indent-delete-char "auto-indent-mode" "" t)
(define-key global-map [remap delete-char] 'auto-indent-delete-char)
(autoload 'auto-indent-kill-line "auto-indent-mode" "" t)
(define-key global-map [remap kill-line] 'auto-indent-kill-line)

(global-set-key (kbd "C-c t") 'neotree-projectile-action)

(provide 'globals)
