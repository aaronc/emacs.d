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
    smex
    evil
    evil-surround
    evil-visualstar
    window-numbering
    editorconfig
    fill-column-indicator
    exec-path-from-shell
    magit
    projectile
    helm-projectile
    company
    helm-company
    idle-highlight-mode
    minimap
    dedicated

    ;;;; Colors & Appearance
    solarized-theme
    color-theme-sanityinc-solarized
    color-theme-sanityinc-tomorrow

    ;;;; Lisp
    smartparens
    paredit
    evil-paredit
    rainbow-delimiters

    ;;;; Clojure
    clojure-mode
    cider
    clj-refactor

    ;; Haskell

    ;; Markdown
    markdown-mode
 
    )
  "cider list of packages to ensure are installed at launch.")

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;; Globals

(prefer-coding-system 'utf-8)

(global-set-key (kbd "C-,") 'other-window)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-S-s") 'save-buffer)
(global-set-key (kbd "C-<") 'previous-buffer)
(global-set-key (kbd "C->") 'next-buffer)

(scroll-bar-mode -1) ;; disables scroll bars
(setq visible-bell 1) ;; disables audible bells & enables visible bell
(tool-bar-mode -1)
(menu-bar-mode -1)

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(global-set-key (kbd "<f7>") 'switch-to-minibuffer-window)

;; indentation

(setq-default indent-tabs-mode nil)
(setq tab-width 2)


;; helm

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

(setq helm-M-x-fuzzy-match 't) 
(setq helm-buffers-fuzzy-matching 't) 
(setq helm-recentf-fuzzy-match 't) 

;; smex
(global-set-key (kbd "M-X") 'smex)

;; ido
(global-set-key (kbd "M-A") 'ido-switch-buffer)
(global-set-key (kbd "M-a") 'helm-mini)

;; evil

(require 'evil)
(require 'evil-visualstar)
(evil-mode 1)
(global-evil-surround-mode 1)

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
(add-hook 'prog-mode-hook 'fci-mode)

;; exec-path-from-shell - fix for OS X path problem
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; magit

(global-set-key (kbd "C-x g") 'magit-status)

;; projectile
(projectile-global-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)

;; company
(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "C-SPC") 'company-complete)

(defun complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

;; (global-set-key (kbd "TAB") 'complete-or-indent)

;; idle-highlight-mode

;;;; Colors & Appearance

(load-theme 'solarized-dark t)

(when (display-graphic-p)
  (if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :font "Consolas-14")
    (set-face-attribute 'default nil :font "Inconsolata-14")))

(defun enable-transparency ()
  (interactive)
  (load-theme 'solarized-dark)
  (set-frame-parameter nil 'alpha '(75 75))
  (set-face-attribute 'default nil :background "black" :foreground "white"))

(defun disable-transparency ()
  (interactive)
  (load-theme 'solarized-dark)
  (set-frame-parameter nil 'alpha '(100 100)))

;;;; Lisp


;; paredit

;; smartparens
(require 'smartparens-config)

(smartparens-global-mode 1)
(show-smartparens-global-mode t)

(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)

(define-minor-mode sp-evil-lisp-mode "Smartparens Evil Lisp Mode"
  :keymap (make-sparse-keymap))
(evil-define-key 'insert sp-evil-lisp-mode-map [backspace] 'sp-backward-delete-char)
(add-hook 'clojure-mode-hook 'sp-evil-lisp-mode)

(define-key sp-keymap (kbd "M-k") 'sp-backward-sexp)
(define-key sp-keymap (kbd "M-j") 'sp-next-sexp)

(define-key sp-keymap (kbd "M-S-j") 'sp-down-sexp)
(define-key sp-keymap (kbd "M-S-k") 'sp-backward-up-sexp)

(define-key sp-keymap (kbd "M-l") 'sp-forward-sexp)
(define-key sp-keymap (kbd "M-h") 'sp-previous-sexp)

(define-key sp-keymap (kbd "M-u") 'sp-up-sexp)
(define-key sp-keymap (kbd "M-d") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "M-B") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "M-W") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "M-w") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-b") 'sp-backward-symbol)

(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)

(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-y") 'sp-copy-sexp)

(define-key sp-keymap (kbd "C-M-l") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-h") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-S-h") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-S-l") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "C-M-S-j") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-M-u") 'sp-splice-sexp-killing-backward)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;;;; Clojure

;; clojure-mode

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

;; cider

(require 'cider)

(setq cider-prompt-save-file-on-load nil)
(setq cider-auto-select-error-buffer nil)
(setq cider-show-error-buffer nil)
(define-key cider-mode-map (kbd "C-c C-v") 'cider-visit-error-buffer)
(cider-repl-toggle-pretty-printing)

;; clj-refactor

(add-hook 'cider-mode-hook (lambda ()
			     ;; (clj-refactor-mode 1)
			     ;; (cljr-add-keybindings-with-prefix "C-x C-r")
                             ))

;;;; Platform-specific stuff

;; for tmux
;; (add-hook 'after-make-frame-functions
;;   (lambda ()
;;     (when (not window-system)
;;       ;;(normal-erase-is-backspace-mode 1)
;;       ;(global-set-key (kbd "C-h") 'delete-backward-char)
;;       )))

;; for OSX
(when (eq system-type 'darwin) ;; mac specific settings
   (setq mac-option-modifier 'alt)
   (setq mac-command-modifier 'meta)
   (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
     )

;; for Windows
(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
   (setenv "PATH" (concat "C:\\gnuwin\\bin;" (getenv "PATH")))
   (setq find-program "C:\gnuwin\\bin\\find.exe"
	 grep-program "C:\\gnuwin\\bin\\grep.exe"))


;;;;;;;; added by Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(flx-ido-mode t)
 '(magit-use-overlays nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Dedicated mode copy
(defvar dedicated-mode nil
  "Mode variable for dedicated minor mode.")
(make-variable-buffer-local 'dedicated-mode)

(defun dedicated-mode (&optional arg)
  "Dedicated minor mode."
  (interactive "P")
  (setq dedicated-mode (not dedicated-mode))
  (set-window-dedicated-p (selected-window) dedicated-mode)
  (if (not (assq 'dedicated-mode minor-mode-alist))
      (setq minor-mode-alist
            (cons '(dedicated-mode " D")
                  minor-mode-alist))))

(define-key cider-mode-map (kbd "C-c C-a")
  (lambda ()
    (interactive)
    (cider-interactive-eval
     "(require 'user)(user/go)")))
