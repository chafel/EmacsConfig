(use-package conf-mode
  :mode (("\\.*rc$" . conf-unix-mode)))


(use-package css-mode
  :config
  (setq css-indent-offset 2)
  (use-package css-comb
    :bind (:map css-mode-map
                ("C-c C-x c" . css-comb))
    :ensure t)

  (use-package rainbow-mode
    :init (add-hook 'css-mode-hook 'rainbow-mode)
    :ensure t)
  )


(use-package flymake
  :diminish flymake-mode)


;; (use-package flycheck
;;   :init (add-hook 'prog-mode-hook 'flycheck-mode)
;;   :ensure t
;;   :config
;;   (setq flycheck-check-syntax-automatically '(mode-enabled save))
;;   (use-package flycheck-elm
;;     :init (add-to-list 'flycheck-checkers 'elm)
;;     :ensure t)
;;   (use-package flycheck-flow
;;     :config
;;     (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
;;     (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
;;     (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
;;     :ensure t)
;;   (use-package flycheck-posframe
;;     :init (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
;;     :ensure t
;;     :config (flycheck-posframe-configure-pretty-defaults)))

(use-package flyspell
  :init
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (use-package ispell
    :config (when (executable-find "hunspell")
              (setq ispell-program-name "hunspell"
                    ispell-really-hunspell t)))
  (use-package flyspell-popup
    :init (bind-key "C-;" 'flyspell-popup-correct flyspell-mode-map)
    :ensure t)
  :diminish flyspell-mode)


(use-package prettier-js
  :init (add-hook 'js2-mode-hook 'prettier-js-mode)
  :ensure t
  :diminish prettier-js-mode)


(use-package sgml-mode
  :config
  (setq sgml-basic-offset 2)
  ;; after deleting a tag, indent properly
  (defadvice sgml-delete-tag (after reindent activate)
    (indent-region (point-min) (point-max))))


(use-package ssh-config-mode
  :mode (("/sshd?_config\\'" . ssh-config-mode)
         ("/\\.ssh/config\\'" . ssh-config-mode)
         ("/known_hosts\\'" . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :ensure t)


(use-package web-mode
  :init (add-hook 'js2-jsx-mode-hook
                  '(lambda()
                     (setq emmet-expand-jsx-className? t)))
  :mode (("\\.hbs$" . web-mode)
         ("\\.html$" . web-mode))
  :ensure t
  :config
  (setq web-mode-style-padding sgml-basic-offset
        web-mode-script-padding sgml-basic-offset
        web-mode-css-indent-offset sgml-basic-offset
        web-mode-code-indent-offset sgml-basic-offset
        web-mode-markup-indent-offset sgml-basic-offset
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-indentation nil
        web-mode-enable-current-column-highlight t
        web-mode-enable-current-element-highlight t))


(use-package emmet-mode
  :ensure t
  :config (dolist (hook '(sgml-mode-hook css-mode-hook web-mode-hook js2-jsx-mode-hook rjsx-mode-hook))
            (add-hook hook #'emmet-mode)))
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
(setq emmet-move-cursor-between-quotes t) ;; default nil
(setq emmet-expand-jsx-className? t) ;; default nil

(use-package markdown-mode
  :mode ("README\\.md$" . gfm-mode)
  :ensure t
  :config (use-package apib-mode
            :mode ("\\.apib$" . apib-mode)
            :ensure t))


(use-package nginx-mode
  :ensure t)


(use-package pkgbuild-mode
  :ensure t)


(use-package pip-requirements
  :ensure t)


(use-package json-mode
  :mode ("\\.json$" . json-mode)
  :ensure t)


(use-package yarn-mode
  :ensure t)


(use-package graphql-mode
  :mode ("\\.gql$" . graphql-mode)
  :ensure t)

;; open new line and indent
(defun wh/open-line-below ()
  "Open a line below the current one (like vim's 'o')."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun wh/open-line-above ()
  "Open a line above the current one (like vim's 'O')."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-for-tab-command))
(global-set-key (kbd "<S-return>") 'wh/open-line-below)
(global-set-key (kbd "<C-S-return>") 'wh/open-line-above)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     nil ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(setq helm-M-x-fuzzy-match t ; optional fuzzy matching for helm-M-x
      helm-buffers-fuzzy-matching           t
      helm-recentf-fuzzy-match              t
      helm-semantic-fuzzy-match             t
      helm-imenu-fuzzy-match t)



(provide 'textmodes)
