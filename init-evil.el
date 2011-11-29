(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)  
(evil-mode 1)

(define-key evil-motion-state-map (kbd "<SPC>") 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "<S-SPC>") 'evil-scroll-page-up)
