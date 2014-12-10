(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      undo-tree
                      clojure-mode
                      clojurescript-mode
                      slime-repl
                      rainbow-delimiters
                      auto-complete
                      ac-slime
                      zenburn-theme
                      evil
                      solarized-theme
                      cider
                      midje-mode
                      surround
                      exec-path-from-shell
                      fill-column-indicator
                      jade-mode
                      editorconfig
		      ac-cider)
   "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
    (when (not (package-installed-p p))
        (package-install p)))

(require 'midje-mode)
(setq fci-rule-column 80)

(add-hook 'text-mode-hook
          (lambda ()
            (fci-mode 1)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (auto-complete-mode 1)
            (midje-mode 1)))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq cider-prompt-save-file-on-load nil)
(setq cider-auto-select-error-buffer nil)

(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (auto-complete-mode 1)))

(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))

(add-hook 'paredit-mode-hook
         (lambda ()
           (define-key evil-insert-state-map (kbd "C-h") 'paredit-backward-delete)))

(defun inf-lisp-switch-ns ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (lisp-eval-defun)))

(global-set-key "\C-c\C-n" 'inf-lisp-switch-ns)

(add-hook 'slime-mode-hook
          (defun slime-save-compile-load ()
            (save-buffer)
            (slime-compile-and-load-file)))

;(set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-13")
(when (display-graphic-p)
  (if (eq system-type 'windows-nt) 
      (set-face-attribute 'default nil :font "Consolas-14")
    (set-face-attribute 'default nil :font "Inconsolata-15")))

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (menu-bar-mode 1)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(global-set-key (kbd "C-,") 'other-window)
(global-set-key (kbd "C-<") 'previous-buffer)
(global-set-key (kbd "C->") 'next-buffer)
(global-set-key [C-S-tab] 'previous-buffer)
(global-set-key [C-tab] 'next-buffer)
(global-set-key "\C- " 'hippie-expand)
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)
(global-set-key [(f12)] 'ibuffer)
(global-set-key [(f11)] 'buffer-menu)
(global-set-key [(f10)] 'ido-switch-buffer)
(global-set-key (kbd "M-a") 'ido-switch-buffer)
(global-set-key (kbd "M-b") 'buffer-menu)


; Map shift-tab to reduce indent
; http://stackoverflow.com/questions/2249955/emacs-shift-tab-to-left-shift-the-block/2250155#2250155
(global-set-key (kbd "<S-tab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

(if load-file-name
   (add-to-list 
      'load-path 
      (file-name-directory (file-truename load-file-name))))

(require 'init-evil)
(require 'init-auto-complete)
(require 'mwe-color-box)

(define-key evil-normal-state-map (kbd "M-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "M-j") 'windmove-down)
(define-key evil-normal-state-map (kbd "M-k") 'windmove-up)
(define-key evil-normal-state-map (kbd "M-l") 'windmove-right)

(define-key evil-normal-state-map (kbd "C-M-f") 'paredit-forward)
(define-key evil-normal-state-map (kbd "C-M-b") 'paredit-backward)
(define-key evil-normal-state-map (kbd "C-M-u") 'paredit-backward-up)
(define-key evil-normal-state-map (kbd "C-M-d") 'paredit-forward-down)
(define-key evil-normal-state-map (kbd "M-d") 'paredit-forward-kill-word)

(define-key evil-normal-state-map (kbd "C-h") 'evil-backward-char)


(require 'window-numbering)
(window-numbering-mode 1)


(require 'moz)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

    (add-hook 'javascript-mode-hook 'javascript-custom-setup)
    (defun javascript-custom-setup ()
      (moz-minor-mode 1))

(setq inferior-lisp-program "script/repl")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("75c9f0b0499ecdd0c856939a5de052742d85af81814e84faa666522c2bba7e85" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" "a7e8dc00fc8043439a738a15e2f593b8e9b2492f" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" "965234e8069974a8b8c83e865e331e4f53ab9e74" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;(load-theme 'zenburn)
(load-theme 'solarized-light)

(require 'auto-complete)
;;; Usage
;; Run M-x moz-reload-mode to switch moz-reload on/off in the
;; current buffer.
;; When active, every change in the buffer triggers Firefox
;; to reload its current page.

(define-minor-mode moz-reload-mode
  "Moz Reload Minor Mode"
  nil " Reload" nil
  (if moz-reload-mode
      ;; Edit hook buffer-locally.
      (add-hook 'post-command-hook 'moz-reload nil t)
    (remove-hook 'post-command-hook 'moz-reload t)))

(defun moz-reload ()
  (when (buffer-modified-p)
    (save-buffer)
    (moz-firefox-reload)))

(defun moz-firefox-reload ()
  (comint-send-string (inferior-moz-process) "BrowserReload();"))

;(require 'auto-save)

(set-face-background 'mode-line "Dark Slate Blue")
(setq evil-normal-state-cursor '("SeaGreen4" box))
(setq evil-insert-state-cursor '("SeaGreen3" bar))
(setq evil-emacs-state-cursor '("red" box))
(define-minor-mode auto-reload-mode
  "Auto Reload Minor Mode"
  nil " Reload" nil
  (if auto-reload-mode
      ;; Edit hook buffer-locally.
      (add-hook 'post-command-hook 'auto-reload nil t)
    (remove-hook 'post-command-hook 'auto-reload t)))

(defun auto-reload ()
  (when (buffer-modified-p)
    (save-buffer)))

(defun piggieback-swap-cljs-repl ()
  (interactive)
  (nrepl-send-request '("op" "piggieback-repl-info")
                      (lambda (res) (print res))))

(require 'editorconfig)

;;(global-set-key (kbd "C-?") 'help-command)

(global-set-key (kbd "C-h") 'delete-backward-char)

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'cider-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
