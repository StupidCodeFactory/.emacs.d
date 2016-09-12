(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq system-uses-terminfo nil)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(scroll-bar-mode 0)
(setq-default fill-column 80)
(setq inhibit-startup-screen t)
(setq large-file-warning-threshold nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)
(global-auto-revert-mode 1)
;;(setq-default line-spacing 1)
(show-paren-mode 1)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)
(setq ns-use-srgb-colorspace t)
(setq linum-format "%4d ")
(tool-bar-mode 0)
(electric-pair-mode t)
(require 'cl)
(toggle-frame-fullscreen)

(require 'ls-lisp)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(load-theme 'wombat t)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


(require 'chruby)
(chruby "2.3.0")
 (when (fboundp 'windmove-default-keybindings)
   (windmove-default-keybindings))

(require 'textmate)
(textmate-mode t)

(require 'projectile)
(projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(setq projectile-completion-system 'grizzl)

(require 'enh-ruby-mode)
(require 'ruby-refactor)
(require 'flymake-ruby)
(require 'haml-mode)
(require 'ruby-block)
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)


(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'rspec-mode)
(add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
(add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode-launch)
(add-hook 'enh-ruby-mode-hook 'flymake-ruby-load)
(add-hook 'enh-ruby-mode-hook 'ruby-block-mode)
(add-hook 'enh-ruby-mode-hook 'ruby-block-highlight-toggle)
(add-hook 'enh-ruby-mode-hook 'ruby-electric-mode)
(setq ruby-deep-indent-paren nil)


(smartscan-mode 1)
(global-linum-mode 1)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

(require 'fullframe)
(fullframe magit-status magit-mode-quit-window nil)

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


(require 'inf-ruby)

;; When folding, take these delimiters into consideration
(add-to-list 'hs-special-modes-alist
             '(enh-ruby-mode
               "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))

(require 'feature-mode)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))
;; I want rspec instead of rake spec
(setq rspec-use-rake-when-possible nil)
;; Scroll to the first test failure
(setq compilation-scroll-output 'first-error)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(require 'yasnippet)
(yas-global-mode 1)

(global-set-key (kbd "s-\\") 'iwb)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; windows rebalancing
 (defadvice split-window-vertically (after rebalance-h nil activate)
   "Calls `balance-windows' after splitting a window with C-x 2."
   (when (interactive)
     (balance-windows)))

 (defadvice delete-window (after rebalance-d nil activate)
   "Calls `balance-windows' after deleting a window with C-x 0."
   (when (interactive)
     (balance-windows)))

(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style "bsd")
 '(company-show-numbers t)
 '(confirm-kill-emacs nil)
 '(enh-ruby-bounce-deep-indent nil)
 '(enh-ruby-deep-indent-paren nil)
 '(make-backup-files nil)
 '(ns-auto-hide-menu-bar t)
 '(projectile-rails-vanilla-command "bin/rails")
 '(rspec-snippets-fg-syntax (quote concise))
 '(rspec-spec-command "rspec")
 '(rspec-use-bundler-when-possible nil)
 '(rspec-use-spring-when-possible nil)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
