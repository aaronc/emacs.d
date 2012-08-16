(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      undo-tree
                      clojure-mode
                      clojure-test-mode
                      clojurescript-mode
                      slime-repl
                      rainbow-delimiters
                      auto-complete
                      ac-slime
                      zenburn-theme)
   "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
    (when (not (package-installed-p p))
        (package-install p)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (clojure-test-mode 1)
            (auto-complete-mode 1)))


(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (auto-complete-mode 1)))

(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))

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
 '(custom-safe-themes (quote ("71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "965234e8069974a8b8c83e865e331e4f53ab9e74" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'zenburn)

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

(set-face-background 'modeline "dark slate blue")
