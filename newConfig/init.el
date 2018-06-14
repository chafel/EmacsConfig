(require 'package)
(add-to-list 'package-archives
             ;;'(("gnu" . "http://elpa.gnu.org/packages/"))
             '("melpa" . "http://melpa.milkbox.net/packages/")
             t)
;;https://melpa.org/packages/ http://melpa.milkbox.net/packages/
(package-initialize)


;; line number
(setq linum-format "%3d")
(defun my-linum-mode-hook ()
  (linum-mode t))
(add-hook 'find-file-hook 'my-linum-mode-hook)

;; theme
(load-theme 'dracula t)

(add-to-list 'default-frame-alist
'(font . "Fira Code 16"))

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

;; auto indent in new line
(define-key global-map (kbd "RET") 'newline-and-indent)

;; package auto install
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/lisp")

;; (require 'pkgs)
(require 'defuns)
;; (load-file "~/.emacs.d/lisp/defuns.el")
(require 'globals)
(require 'orgmodes)
(require 'progmodes)
(require 'textmodes)
(require 'othermodes)


(provide 'init)
;;; init.el ends here
