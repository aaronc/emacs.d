;; package

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
  '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Add in your own as you wish:
(defvar my-packages
  '(
    ;;;; Globals
    helm
    evil
    window-numbering
    editorconfig
    fill-column-indicator
    exec-path-from-shell
    magit
    projectile
    helm-projectile

    ;;;; Colors & Appearance
    solarized-theme
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow

    ;;;; Lisp
    smartparens
    rainbow-delimiters

    ;;;; Clojure
    clojure-mode
    A
    )
   "cider list of packages to ensure are installed at launch.")

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
    (when (not (package-installed-p p))
        (package-install p)))

;;;; Globals

(global-set-key (kbd "C-,") 'other-window)
(scroll-bar-mode -1) ;; disables scroll bars
(setq visible-bell 1) ;; disables audible bells & enables visible bell

;; evil

(require 'evil)
(evil-mode 1)

;; helm

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

;; ido
(global-set-key (kbd "M-a") 'ido-switch-buffer)

;; window-numbering

(require 'window-numbering)
(window-numbering-mode 1)

;; editorconfig

(require 'editorconfig)

;; fill-column-indicator

(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq whitespace-style '(face trailing))
(setq fci-rule-color "#DDD")
(add-hook 'text-mode-hook 'fci-mode)

;; exec-path-from-shell - fix for OS X path problem
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; magit

;; projectile

(projectile-global-mode)

;;;; Colors & Appearance

(when (display-graphic-p)
  (if (eq system-type 'windows-nt) 
      (set-face-attribute 'default nil :font "Consolas-14")
    (set-face-attribute 'default nil :font "Inconsolata-15")))

(load-theme 'solarized-light)

;;;; Lisp

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;;;; Clojure

;; clojure-mode

;; cider


;;;; Platform-specific stuff

;;;;;;;; added by Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
