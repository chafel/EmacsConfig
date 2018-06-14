(use-package org
  :init
  (add-hook 'org-mode-hook
            (lambda ()
	      (toggle-truncate-lines))))

(provide 'orgmodes)
