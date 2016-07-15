;; (load "~/emacs-haskell-config/init.el")
;; package

(require 'package)
(add-to-list 'package-archives
            '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
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
    ;;company-flx
    auto-complete
    helm-company
    idle-highlight-mode
    minimap
    dedicated
    smart-mode-line

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
    typed-clojure-mode

    ;; Haskell
    haskell-mode
    shm
    hindent
    company-ghc
    flymake-hlint
    intero
    ;;flycheck-haskell

    ;; Idris
    idris-mode
    popwin
    helm-idris

    ;; org
    org
    org-plus-contrib
    htmlize

    ;; etc
    yasnippet
    markdown-mode
    yaml-mode
    dockerfile-mode
    scss-mode
    emmet-mode
    writeroom-mode
    web-mode
    flycheck-flow
    ;;tide
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
(setq-default tab-width 2)
(setq c-basic-offset 2)
(setq js-indent-level 2)

;; helm

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

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

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
;; (yas-global-mode 1)

;; company
(require 'company)

(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'company-backends '(company-capf company-dabbrev-code))

(global-set-key (kbd "TAB") 'company-indent-or-complete-common)
(global-set-key (kbd "C-SPC") 'company-complete)

(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map [tab] 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)
(setq company-require-match 'never)
(setq company-auto-complete t)
(setq company-auto-complete-chars 1)
(setq company-idle-delay 0.0)
(setq company-echo-delay 0)
(setq company-tooltip-limit 25) 
(setq company-show-numbers t)
(setq company-tooltip-margin 1)
(setq company-tooltip-minimum-width 30)
(add-to-list 'completion-styles 'initials t)

;;(require 'company-flx)
;;(with-eval-after-load 'company
;;  (company-flx-mode +1))

(require 'auto-complete)
;; (add-hook 'after-init-hook 'global-auto-complete-mode)
;; (global-set-key (kbd "TAB") 'auto-complete)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map (kbd "C-n") 'ac-next)


;; idle-highlight-mode

;;;; Colors & Appearance

(load-theme 'solarized-light t)

(when (display-graphic-p)
  (if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :font "Consolas-14")
    (set-face-attribute 'default nil :font "Inconsolata-20" :bold 't)))

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

;; (smartparens-global-mode 1)
;; (show-smartparens-global-mode t)

(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'haskell-mode-hook 'smartparens-strict-mode)

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

(require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

(define-clojure-indent
  (ann-protocol 1)
  (data-case 1))

(define-key clojure-mode-map (kbd "C-c C-j") 'org-babel-tangle-jump-to-org)

;; cider

(require 'cider)

(setq cider-prompt-save-file-on-load nil)
(setq cider-auto-select-error-buffer nil)
(setq cider-show-error-buffer 'except-in-repl)
(define-key cider-mode-map (kbd "C-c C-v") 'cider-visit-error-buffer)
(define-key cider-repl-mode-map (kbd "C-c C-v") 'cider-visit-error-buffer)
(setq cider-repl-use-pretty-printing nil)

;; clj-refactor

(add-hook 'cider-mode-hook (lambda ()
			     ;; (clj-refactor-mode 1)
			     ;; (cljr-add-keybindings-with-prefix "C-x C-r")
                             ))
;; typed-clojure-mode

(add-hook 'clojure-mode-hook 'typed-clojure-mode)

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
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(flx-ido-mode t)
 ;; '(haskell-interactive-mode-eval-mode (quote haskell-mode))
 ;; '(haskell-interactive-mode-eval-pretty nil)
 ;; '(haskell-interactive-mode-include-file-name nil)
 ;; '(haskell-notify-p t)
 ;; '(haskell-process-args-cabal-repl (quote ("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng")))
 ;; '(haskell-process-args-ghci (quote ("-ferror-spans")))
 ;; '(haskell-process-args-stack-ghci
 ;;   (quote
 ;;    ("--ghc-options=-ferror-spans" "--with-ghc=ghci-ng")))
 ;; '(haskell-process-auto-import-loaded-modules t)
 ;; '(haskell-process-log t)
 ;; '(haskell-process-path-ghci "ghci-ng")
 ;; '(haskell-process-reload-with-fbytecode nil)
 ;; '(haskell-process-suggest-haskell-docs-imports t)
 ;; '(haskell-process-suggest-remove-import-lines nil)
 ;; '(haskell-process-type (quote stack-ghci))
 ;; '(haskell-process-use-presentation-mode t)
 ;; '(haskell-stylish-on-save nil)
 ;; '(haskell-tags-on-save nil)
 '(idris-hole-show-on-load nil)
 '(magit-use-overlays nil)
 '(safe-local-variable-values
   (quote
    ((org-src-preserve-indentation . t)
     (org-html-validation-link))))
 '(shm-auto-insert-bangs t)
 '(shm-auto-insert-skeletons t)
 '(shm-use-presentation-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fdf6e3" :foreground "#657b83" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 161 :width normal :foundry "nil" :family "Inconsolata")))))

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

;; (define-key cider-mode-map (kbd "C-c C-a")
;;   (lambda ()
;;     (interactive)
;;     (cider-interactive-eval
;;      "(require 'user)(user/go)")))

;; markdown

(add-hook 'markdown-mode 'visual-line-mode)

;; org mode

(require 'org)
;; (require 'ob-clojure)

(define-key org-mode-map (kbd "M-a") 'helm-mini)
(define-key org-mode-map (kbd "C-c C-h") 'org-html-export-to-html)
(define-key org-mode-map (kbd "C-c C-z") 'cider-switch-to-repl-buffer)
;; (define-key org-mode-map (kbd "C-c e") 'org-edit-special)

;; No need to prompt on each evaluation
(setq org-confirm-babel-evaluate nil)
;; Turn off annoying html validation link in export
(setq org-html-validation-link nil)
;; Preserve source indentation when using edit buffer
(setq org-src-preserve-indentation 't)
;; Set tangled files to read-only by default
(setq org-babel-default-header-args
      (cons '(:tangle-mode . #o444)
            (assq-delete-all :tangle-mode org-babel-default-header-args)))
;; Make parent directories where needed by default
(setq org-babel-default-header-args
      (cons '(:mkdirp . "yes")
            (assq-delete-all :mkdirp org-babel-default-header-args)))
;; Relative paths don't seem to be working right
(setq org-babel-tangle-use-relative-file-links nil)
;; edit in current window
(setq org-src-window-setup 'current-window)
(setq org-src-ask-before-returning-to-edit-buffer nil)


;; (setq org-babel-clojure-backend 'cider)

(require 'ob)

(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure
  '((:results . "silent")))

(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with Babel."
  (cider-interactive-eval body))

(defun org-src-mode-clojure-hook ()
  (if org-src--overlay
    (set (make-local-variable 'cider-buffer-ns)
         (with-current-buffer (overlay-buffer org-src--overlay) cider-buffer-ns))
    (fci-mode -1)))

(add-hook 'org-src-mode-hook 'org-src-mode-clojure-hook)

(provide 'ob-clojure)

(setq org-babel-default-header-args:clojure
      (cons '(:comments . "noweb")
            (assq-delete-all :comments org-babel-default-header-args:clojure)))

(defun post-tangle ()
  (let ((filename (buffer-file-name (current-buffer))))
    (print "post-tangle")
    (print filename)
    (when (string-match "^.*\.clj[cs]?$" filename)
      (cider-load-file filename)
      ;; (cider-load-buffer (current-buffer))
      ;; (cider-interactive-eval
      ;;  ;; "(when-let [check-ns (resolve 'clojure.core.typed/check-ns)] (check-ns))"
      ;;  "(clojure.test/run-tests)")
      )))

(add-hook 'org-babel-post-tangle-hook 'post-tangle)

(add-to-list 'org-babel-tangle-lang-exts '("idris" . "idr"))

(require 'ob-sh)

(org-babel-do-load-languages 'org-babel-load-languages '((sh . t)))

;; smart-mode-line
(require 'smart-mode-line)

(setq powerline-arrow-shape 'curve)
(setq powerline-default-separator-dir '(left . left))
;;(setq sml/theme 'solarized)
(sml/setup)

;; haskell

(require 'shm)
(require 'hindent)
(require 'shm-case-split)
(require 'shm-reformat)
(require 'haskell)
(require 'haskell-mode)
(require 'hindent)
(require 'haskell-process)
;;(require 'haskell-simple-indent)
(require 'haskell-interactive-mode)
(require 'haskell-font-lock)

;;(ghc-comp-init)
;; (add-to-list 'company-backends 'company-ghc)



(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'intero-mode)
;; (add-hook 'haskell-mode-hook 'flymake-hlint-load)
;; (add-hook 'haskell-interactive-mode-hook 'structured-haskell-repl-mode)
;; (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

;; (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
(setq company-ghc-show-info nil)
(setq company-ghc-show-module 't)

;; Keybindings

(define-key shm-map (kbd "M-a") 'helm-mini)
(define-key shm-repl-map (kbd "M-a") 'helm-mini)
(defun company-auto-complete-or-shm/tab ()
  (interactive)
  (cond
   ((save-excursion (goto-char (line-beginning-position))
                    (looking-at "^[ ]*$"))
    (shm/tab))
   (t (company-complete))))
(define-key shm-map (kbd "TAB") 'company-auto-complete-or-shm/tab)
(define-key haskell-interactive-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

(set-face-background 'shm-current-face "#eee8d5")
(set-face-background 'shm-quarantine-face "lemonchiffon")

;; idris

(require 'popwin)
(setq idris-enable-elab-prover 't)
(push 'idris-compiler-notes-mode
      popwin:special-display-config)
(push '(idris-repl-mode
        :height 0.2
        :noselect nil
        :position bottom
        :stick t)
      popwin:special-display-config)
(add-hook 'idris-mode-hook 'popwin-mode)
(add-hook 'idris-mode-hook 'auto-complete-mode)

;; writeroom

(setq writeroom-width 120)

;; (load "./ebnf-mode.el")

;; Typescript

;; sample config
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            ;; company is an optional dependency. You have to
            ;; install it separately via package-install
            (company-mode-on)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil :tabSize 2))
;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options

;; Tide can be used along with web-mode to edit tsx files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1)
              (company-mode-on))))

(require 'flycheck-flow)
(add-hook 'javascript-mode-hook 'flycheck-mode)
