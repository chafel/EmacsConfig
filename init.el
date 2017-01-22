(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
;; http://melpa.milkbox.net/packages/
(package-initialize)

;; line number
(setq linum-format "%4d")
(defun my-linum-mode-hook ()
  (linum-mode t))
(add-hook 'find-file-hook 'my-linum-mode-hook)

(add-to-list 'default-frame-alist
             '(font . "Fira Code 16"))

;; remap close key
(global-set-key (kbd "C-x C-c")      'keyboard-escape-quit)

;; (mac-auto-operator-composition-mode)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq auto-save-interval 2000)
(setq auto-save-timeout 1200)

(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(setq git-commit-summary-max-length 80)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/site-lisp/magit/Documentation/"))

;; (add-to-list 'load-path "~/.emacs.d/tern/")
;; (autoload 'tern-mode "tern.el" nil t)
;; (add-hook 'js-mood-hook (lambda () (tern-mode t)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))

(require 'multiple-cursors)
(global-set-key (kbd "C-x C-d") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x C-,") 'mc/mark-all-like-this)
;; If you want to insert a newline in multiple-cursors-mode, use C-j or C-m

(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
;; ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;; (global-set-key (kbd "C-c p s a") 'helm-projectile-ack)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     nil ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(setq helm-M-x-fuzzy-match t ; optional fuzzy matching for helm-M-x
      helm-buffers-fuzzy-matching           t
      helm-recentf-fuzzy-match              t
      helm-semantic-fuzzy-match             t
      helm-imenu-fuzzy-match                t)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

(setq helm-split-window-in-side-p t)

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(helm-mode 1)
(helm-autoresize-mode t)
(load "~/.emacs.d/helm-swoop/helm-swoop")

(require 'helm-projectile)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
;; (setq projectile-switch-project-action 'helm-projectile-find-file)
(setq projectile-switch-project-action 'helm-projectile)
;; (setq projectile-enable-caching t)

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
 ("C-c `"  . wrap-with-back-quotes))

(sp-pair "<_>" "</_>")

;; less mode
(load "~/.emacs.d/less-css-mode/less-css-mode")

;; acejump
(global-set-key (kbd "C-x j") 'ace-jump-mode)
(global-set-key (kbd "C-x c") 'ace-jump-char-mode)

;; ace-window
(global-set-key (kbd "M-p") 'ace-window)

;; helm-swoop
(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match t)

;; If you would like to use migemo, enable helm's migemo feature
;; (helm-migemo-mode 1)

;; git-gutter
(require 'git-gutter)
;; (global-git-gutter-mode +1)

(global-set-key (kbd "C-x
C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
;; Mark current hunk
(global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk)

;; (load "~/.emacs.d/find-file-in-project/find-file-in-project")
;; (autoload 'find-file-in-project "find-file-in-project" nil t)
;; (autoload 'find-file-in-project-by-selected "find-file-in-project" nil t)
;; (autoload 'find-directory-in-project-by-selected "find-file-in-project" nil t)
;; (autoload 'ffip-show-diff "find-file-in-project" nil t)
;; (autoload 'ffip-save-ivy-last "find-file-in-project" nil t)
;; (autoload 'ffip-ivy-resume "find-file-in-project" nil t)
;; (setq ffip-project-root "~/beary/pensieve")

;; autosave files in a auto-save directory
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;; backup files in a backups directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; project-explore
(use-package project-explorer
  :ensure t
  :config (progn (setq pe/omit-gitignore t)
                 (setq pe/width 32)))

(use-package key-chord
  :ensure t
  :init (key-chord-mode 1)
  :config (progn (key-chord-define-global "$$" 'project-explorer-open)
                 (key-chord-define-global "xx" 'execute-extended-command)))

;; show whitespace/tab
(setq whitespace-style
      '(face indentation::tab indentation::space tabs tab-mark trailing))
(whitespace-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; smart indent space instead of tab
;; use C-q TAB if really want to insert tab
(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

;; migrate form https://github.com/L42y/.emacs.d/blob/master/init.el
;; (add-to-list 'load-path "~/.emacs.d/lisp")

;; (require 'pkgs)
;; (require 'defuns)
(load-file "~/.emacs.d/lisp/defuns.el")
;; (require 'globals)
;; (require 'orgmodes)
;; (require 'progmodes)
;; (require 'textmodes)
;; (require 'othermodes)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")
(load-file "~/.emacs.d/themes/color-theme-tomorrow.el")
(setq custom-safe-themes t)

;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (let ((mode (if (display-graphic-p frame) 'light 'dark)))
;;               (set-frame-parameter frame 'background-mode mode)
;;               (set-terminal-parameter frame 'background-mode mode))
;;             (enable-theme 'solarized)))
(set-terminal-parameter nil 'background-mode 'light)
;; (load-theme 'solarized t)
(load-theme 'monokai t)

;; highlight column: col-highlight

;; auto indent mode
(setq auto-indent-on-visit-file t)
;; If you want auto-indent on for files
(require 'auto-indent-mode)
(auto-indent-global-mode)
(setq auto-indent-assign-indent-level 2)

(defun my-setup-indent (n)
  ;; java/c/c++
  (setq c-basic-offset n)
  ;; web development
  (setq coffee-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js2-indent-switch-body t)
  (setq js-switch-indent-offset 2)
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  ;; (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  )

(my-setup-indent 2)

;; (use-package aggressive-indent
;;   :ensure t
;;   :config (global-aggressive-indent-mode 1))

(setq indent-tabs-mode nil)
(infer-indentation-style)

(add-hook 'before-save-hook
          (lambda ()
            (untabify (point-min) (point-max))
            ))

;; duplicate current line
(defun duplicate-current-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
          (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
        (insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
        (insert current-line)
        (decf n)))))

(global-set-key (kbd "C-S-d") 'duplicate-current-line)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(case-fold-search nil)
 '(coffee-indent-level 2)
 '(compilation-message-face (quote default))
 '(css-indent-level 2)
 '(custom-safe-themes
   (quote
    ("427fed191e7a766152e59ef0e2904283f436dbbe259b9ccc04989f3acde50a55" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "fe230d2861a13bb969b5cdf45df1396385250cc0b7933b8ab9a2f9339b455f5c" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" default)))
 '(fci-rule-color "#20240E")
 '(frame-background-mode (quote dark))
 '(fringe-mode (quote (0)) nil (fringe))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#20240E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#20240E" . 100))))
 '(js-indent-level 2)
 '(magit-diff-arguments (quote ("--no-ext-diff" "--stat")))
 '(magit-diff-use-overlays nil)
 '(magit-log-section-arguments (quote ("--decorate" "-n256")))
 '(magit-rebase-arguments nil)
 '(package-selected-packages
   (quote
    (smart-comment ace-window auto-indent-mode auto-complete rainbow-delimiters rainbow-identifiers use-package smartparens project-explorer paredit multiple-cursors monokai-theme magit key-chord js2-mode ido-ubiquitous highlight-parentheses helm-projectile helm-ag git-gutter expand-region emmet-mode dracula-theme col-highlight coffee-mode aggressive-indent ace-jump-mode 0blayout)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(standard-indent 2)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#20240E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(defun auto-complete-mode-maybe ()
  "No maybe for you. Only AC!"
  (auto-complete-mode 1))
(ac-set-trigger-key "TAB")
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;; company mode
;; (global-set-key (kbd "C-q") 'company-complete)
;; (add-hook 'after-init-hook 'global-company-mode)
;; (setq company-require-match nil)

(use-package expand-region
  :ensure t
  :config (global-set-key (kbd "C-=") 'er/expand-region))

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(show-paren-mode 1)
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))


;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-extra-auto-pairs
;;         '(("erb"  . (("beg" "end")))
;;           ("php"  . (("beg" "end")
;;                      ("beg" "end")))
;;           ))

;;   (setq web-mode-ac-sources-alist
;;         '(("css" . (ac-source-css-property))
;;           ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;; )
;; (add-hook 'web-mode-hook  'my-web-mode-hook)
(put 'scroll-left 'disabled nil)

(load-file "~/.emacs.d/lisp/sgml-mode-patch.el")
(require 'sgml-mode)

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

;; emmet
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'js-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'js2-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'jsx-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'js2-jsx-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
(setq emmet-move-cursor-between-quotes t) ;; default nil
(setq emmet-expand-jsx-className? t) ;; default nil
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

;; (require 'desktop)
;; (desktop-save-mode 1)
;; (setq history-length 50)
;; (setq desktop-auto-save-timeout 3600)
;; (add-to-list 'desktop-globals-to-save 'file-name-history)
;; (setq desktop-buffers-not-to-save
;;       (concat "\\("
;;               "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;;               "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
;;               "\\)$"))
;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
;; (add-to-list 'desktop-modes-not-to-save 'Info-mode)
;; (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;; (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
;; ;;; desktop-override-stale-locks.el begins here
;; (defun emacs-process-p (pid)
;;   "If pid is the process ID of an emacs process, return t, else nil.
;; Also returns nil if pid is nil."
;;   (when pid
;;     (let ((attributes (process-attributes pid)) (cmd))
;;       (dolist (attr attributes)
;;         (if (string= "comm" (car attr))
;;             (setq cmd (cdr attr))))
;;       (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(global-set-key (kbd "M-s") 'save-buffer)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
