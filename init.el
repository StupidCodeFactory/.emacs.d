(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(scroll-bar-mode 0)
(setq-default fill-column 80)
(setq inhibit-startup-screen t)
(setq large-file-warning-threshold nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)
(global-auto-revert-mode 1)
(setq-default line-spacing 1)
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
(setq ns-use-srgb-colorspace nil)
(tool-bar-mode 0)
(electric-pair-mode t)
(require 'ls-lisp)
(cua-mode 0)
(transient-mark-mode 1) ;; No region when it is not highlighted
(global-linum-mode 1)

;; (set-face-attribute 'default nil :height 140)
(set-face-attribute 'default nil :height 110)

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defun colorize-compilation-buffer ()
  "Colorize compilation buffer."
  (interactive)
  (read-only-mode)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-to-list 'load-path "~/.emacs.d/powerline")

(use-package exec-path-from-shell
  :ensure t)

(use-package chruby
  :ensure t
  :init
  (chruby-use-corresponding)
  )

(use-package clojure-mode
  :ensure t
  :init
  (clojure-mode)
  )

(use-package cider
  :ensure t
  )

  (use-package projectile
    :ensure t
    :config
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1)
    (projectile-mode)
    (add-hook 'projectile-mode-hook 'projectile-rails-on)
    (setq projectile-cache-file (expand-file-name ".projectile.cache" user-emacs-directory))
    (setq projectile-known-projects-file (expand-file-name ".projectile-bookmarks.eld" user-emacs-directory))
    (setq projectile-sort-order (quote recently-active))
    (setq projectile-enable-caching t)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'hybrid)
    (setq projectile-sort-order 'access-time)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store"))

  (use-package company
    :ensure t
    :defer t
    :init (global-company-mode)
    (progn
      ;; Use Company for completion
      (bind-key [remap completion-at-point] #'company-complete company-mode-map)
      (setq company-tooltip-align-annotations t
            ;; Easy navigation to candidates with M-<n>
            company-show-numbers t)
      (push 'company-robe company-backends)
      )
    :diminish company-mode
    )


  (use-package textmate :ensure t :config (textmate-mode))

  (use-package direnv
    :ensure t
    :config
    (setq direnv-always-show-summary t))

  (use-package counsel
    :after ivy
    :config (counsel-mode))

  (use-package ivy
    :ensure t
    :defer 0.1
    :diminish
    :bind (("C-c C-r" . ivy-resume)
           ("C-x B" . ivy-switch-buffer-other-window))
    :custom
    (ivy-count-format "(%d/%d) ")
    (ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    :config (ivy-mode))

  (use-package ivy-rich
    :after counsel
    :init (setq ivy-rich-path-style 'abbrev
                ivy-virtual-abbreviate 'full)
    :config (ivy-rich-mode))

  (use-package swiper
    :ensure t
    :after ivy
    :bind (("C-s" . swiper)
           ("C-r" . swiper)))

  (use-package company
    :no-require t
    :config
    (push 'company-robe company-backends))

  (use-package powerline :ensure t)

  (use-package inf-ruby :ensure t)

  ;; (use-package smartparens
  ;;   :ensure t
  ;;   :diminish (smartparens-mode .  "()")
  ;;   :init
  ;;   (use-package smartparens-ruby)
  ;;   (use-package smartparens-config)
  ;;   (use-package smartparens-html)
  ;;   (use-package smartparens-markdown)
  ;;   (use-package smartparens-javascript)
  ;;   (add-hook 'ruby-mode-hook 'smartparens-strict-mode))

  (use-package ruby-mode
    :defer 0.5
    :ensure t
    :ensure flycheck
    :init
    :mode (("\\.rb\\'"       . ruby-mode)
           ("\\.ru\\'"       . ruby-mode)
           ("\\.jbuilder\\'" . ruby-mode)
           ("\\.gemspec\\'"  . ruby-mode)
           ("\\.rake\\'"     . ruby-mode)
           ("Rakefile\\'"    . ruby-mode)
           ("Gemfile\\'"     . ruby-mode)
           ("Guardfile\\'"   . ruby-mode)
           ("Capfile\\'"     . ruby-mode)
           ("Vagrantfile\\'" . ruby-mode))
    :config (progn
              (setq ruby-indent-level 2
                    ruby-indent-tabs-mode nil
                    ruby-insert-encoding-magic-comment nil
                    ruby-insert-encoding-magic-comment nil
                    ruby-deep-indent-paren nil)
              ))

  (use-package projectile-rails
    :after projectile
    :ensure t
    :diminish projectile-rails-mode
    :hook (projectile-mode . projectile-rails-global-mode)
    :config
    (progn
      (setq projectile-rails-vanilla-command "bin/rails"
            projectile-rails-spring-command "bin/spring"
            projectile-rails-zeus-command "bin/zeus"))
    )

  (use-package flycheck
    :ensure t
    :defer 5
    :config
    :init (global-flycheck-mode))

  (use-package ruby-refactor :ensure t)
  (use-package rspec-mode
    :diminish rspec-mode
    :ensure t
    :commands rspec-install-snippets
    :hook (dired-mode . rspec-dired-mode)
    :config (with-eval-after-load 'yasnippet
              (rspec-install-snippets)))


  (use-package web-mode
    :ensure t
    :mode (("\\.erb\\'" . web-mode)
           ("\\.mustache\\'" . web-mode)
           ("\\.html?\\'" . web-mode)
           ("\\.php\\'" . web-mode))
    :config (progn
              (setq web-mode-markup-indent-offset 2
                    web-mode-css-indent-offset 2
                    web-mode-code-indent-offset 2)))

  (use-package helm-projectile
    :ensure t
    :bind ("M-t" . helm-projectile-find-file)
    :config
    (helm-projectile-on))

  (use-package ido
    :config
    (setq ido-enable-flex-matching t)
    (ido-mode 1))

  (use-package robe
    :ensure t
    :bind ("C-M-." . robe-jump)

    :init
    (add-hook 'ruby-mode-hook 'robe-mode))

  (use-package rspec-mode
    :ensure t
    :after ruby-mode
    :disabled t)

  (use-package bundler :ensure t)

  (use-package rubocop
    :ensure t
    :init
    (add-hook 'ruby-mode-hook 'rubocop-mode)
    :diminish rubocop-mode)

  (use-package minitest :ensure t)
  (use-package ruby-tools
    :ensure t
    :init
    (add-hook 'ruby-mode-hook 'ruby-tools-mode)
    :diminish ruby-tools-mode)

  (use-package darktooth-theme :ensure t)

  (use-package slim-mode
    :mode (("\\.slim$" . slim-mode))
    :ensure t)

  (require 'powerline)
  (powerline-default-theme)

  (load-theme 'darktooth t)

  (use-package yasnippet
    :ensure t
    :init
    (progn
      (yas-global-mode 1)
      (setq yas-snippet-dirs
            '("~/.emacs.d/snippets"))))

  (use-package yasnippet-snippets
    :ensure t)

  (use-package ivy
    :ensure t
    :init
    (progn
      (ivy-mode 1)
      (setq ivy-use-virtual-buffers t)
      :config
      (setq ivy-wrap t)
      (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
      (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
      )
    )

  (use-package flx-ido
    :ensure t
    :init (flx-ido-mode 1))

  (use-package ido-vertical-mode
    :ensure t
    :init (ido-vertical-mode 1))


  (use-package counsel-projectile
    :after ivy
    :ensure t
    :config
    (counsel-projectile-mode)
    )

  (use-package beacon
    :ensure t
    :config
    (progn
      (beacon-mode 1)
      ))

  (use-package dumb-jump
    :ensure t)

  (use-package exato :ensure t)

  (require 'ansi-color)
  ;; (defun my/ansi-colorize-buffer ()
  ;;   (let ((buffer-read-only nil))
  ;;     (ansi-color-apply-on-region (point-min) (point-max))))
  ;; (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings))

  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)

  (require 'whitespace)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  (use-package haml-mode :ensure t :defer t)

  (use-package yaml-mode :ensure t)

  (use-package less-css-mode :ensure t)

  (use-package web-mode :ensure t)

  (use-package magit
    :ensure t
    :bind (("C-x g" . magit-status))
    :config
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
    )

  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)

  (defun iwb ()
    "indent whole buffer."
    (interactive)
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil))

  (global-set-key (kbd "s-\\") 'iwb)
  ;; windows rebalancing
  (defadvice split-window-vertically (after rebalance-h nil activate)
    "Calls `balance-windows' after splitting a window with C-x 2."
    (when (called-interactively-p 'interactive)
      (balance-windows)))

  (defadvice split-window-horizontally (after rebalance-v nil activate)
    "Calls `balance-windows' after splitting a window with C-x 3."
    (when (called-interactively-p 'interactive)
      (balance-windows)))

  (defadvice delete-window (after rebalance-d nil activate)
    "Calls `balance-windows' after deleting a window with C-x 0."
    (when (called-interactively-p 'interactive)
      (balance-windows)))

  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))


  ;; (require 'web-mode)
  ;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))
  ;; (setq web-mode-content-types-alist
  ;;       '(("jsx" . "\\.js[x]?\\'")
  ;;         ))
  ;; (add-hook 'web-mode-hook
  ;;           (lambda ()
  ;;             (if (equal web-mode-content-type "javascript")
  ;;                 (web-mode-set-content-type "jsx")
  ;;               (message "now set to: %s" web-mode-content-type))))

  ;; ;; (require 'solidity-mode)
  ;; (require 'js2-mode)

  ;; (require 'mocha)

  ;; (require 'rg)
  ;; (rg-enable-default-bindings (kbd "M-s"))

  ;; (set-face-attribute 'default (selected-frame) :height 100)

  ;; (require 'ensime)
  ;; (require 'sbt-mode)
  ;; (require 'scala-mode)
  ;; (elpy-enable)
  ;; (require 'ein)

  ;; (add-hook 'python-mode-hook 'jedi:setup)
  ;; (setq jedi:complete-on-dot t)
  ;; (setq jedi:environment-root "jedi")

  ;; (setq python-shell-interpreter "jupyter"
  ;;       python-shell-interpreter-args "console --simple-prompt"
  ;;       python-shell-prompt-detect-failure-warning nil)
  ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
  ;;              "jupyter")

  (windmove-default-keybindings 'super)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon-blink-when-focused t)
 '(beacon-color "light green")
 '(beacon-size 20)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "bsd"))))
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("88049c35e4a6cedd4437ff6b093230b687d8a1fb65408ef17bfcf9b7338734f6" "834cbeacb6837f3ddca4a1a7b19b1af3834f36a701e8b15b628cad3d85c970ff" "d3e333eaa461c82a124f7e7a7a9637d56fc3019478becb1847952804ca67743e" "47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(editorconfig-indentation-alist
   (quote
    ((nil)
     (awk-mode c-basic-offset)
     (c++-mode c-basic-offset)
     (c-mode c-basic-offset)
     (cmake-mode cmake-tab-width)
     (coffee-mode coffee-tab-width)
     (cperl-mode cperl-indent-level)
     (crystal-mode crystal-indent-level)
     (css-mode css-indent-offset)
     (emacs-lisp-mode lisp-indent-offset)
     (erlang-mode erlang-indent-level)
     (ess-mode ess-indent-offset)
     (feature-mode feature-indent-offset feature-indent-level)
     (fsharp-mode fsharp-continuation-offset fsharp-indent-level fsharp-indent-offset)
     (groovy-mode c-basic-offset)
     (haskell-mode haskell-indent-spaces haskell-indent-offset haskell-indentation-layout-offset haskell-indentation-left-offset haskell-indentation-starter-offset haskell-indentation-where-post-offset haskell-indentation-where-pre-offset shm-indent-spaces)
     (idl-mode c-basic-offset)
     (jade-mode jade-tab-width)
     (java-mode c-basic-offset)
     (js-mode js-indent-level)
     (js-jsx-mode js-indent-level sgml-basic-offset)
     (js2-mode js2-basic-offset)
     (js2-jsx-mode js2-basic-offset sgml-basic-offset)
     (js3-mode js3-indent-level)
     (json-mode js-indent-level)
     (julia-mode julia-indent-offset)
     (latex-mode . editorconfig-set-indentation/latex-mode)
     (lisp-mode lisp-indent-offset)
     (livescript-mode livescript-tab-width)
     (lua-mode lua-indent-level)
     (matlab-mode matlab-indent-level)
     (mustache-mode mustache-basic-offset)
     (nginx-mode nginx-indent-level)
     (nxml-mode nxml-child-indent
                (nxml-attribute-indent . 2))
     (objc-mode c-basic-offset)
     (octave-mode octave-block-offset)
     (perl-mode perl-indent-level)
     (php-mode c-basic-offset)
     (pike-mode c-basic-offset)
     (ps-mode ps-mode-tab)
     (puppet-mode puppet-indent-level)
     (python-mode . editorconfig-set-indentation/python-mode)
     (ruby-mode ruby-indent-level)
     (rust-mode rust-indent-offset)
     (scala-mode scala-indent:step)
     (scss-mode css-indent-offset)
     (sgml-mode sgml-basic-offset)
     (sh-mode sh-basic-offset sh-indentation)
     (slim-mode slim-indent-offset)
     (tcl-mode tcl-indent-level tcl-continued-indent-level)
     (typescript-mode typescript-indent-level)
     (verilog-mode verilog-indent-level verilog-indent-level-behavioral verilog-indent-level-declaration verilog-indent-level-module verilog-cexp-indent verilog-case-indent)
     (web-mode
      (web-mode-indent-style lambda
                             (size)
                             2)
      web-mode-markup-indent-offset web-mode-css-indent-offset web-mode-code-indent-offset web-mode-block-padding web-mode-script-padding web-mode-style-padding)
     (yaml-mode yaml-indent-offset))))
 '(editorconfig-mode t)
 '(ensime-startup-notification nil)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(frame-brackground-mode (quote dark))
 '(global-whitespace-newline-mode nil)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (cider slime-repl swank-clojure npm-mode ivy-rich smartparens jinja2-mode slim slim-theme company-anaconda minitest robe docker-compose-mode flymake-elixir elixir-yasnippets elixir-mode elpy company-jedi jedi ein virtualenv auto-virtualenvwrapper cargo flycheck-rust rust-auto-use rust-mode s typescript-mode gruber-darker-theme darktooth-theme color-theme zenburn-theme hc-zenburn-theme elein clojure-mode-extra-font-locking clojure-snippets clojure-mode rubocop slim-mode powerline slack counsel-projectile counsel ivy yaml-mode ruby-tools ruby-electric hcl-mode terraform-mode ensime ripgrep mocha mocha-snippets company-solidity package-build epl git commander f dash go-mode emmet-mode color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow karma dockerfile-mode docker editorconfig ecukes cask-mode cask emamux dracula-theme nginx-mode direnv markdown-mode+ markdown-preview-mode flymd markdown-mode jsx-mode jdee ctags-update grizzl bundler ag company ruby-dev ruby-block haml-mode fullframe flymake-ruby flx-ido feature-mode exec-path-from-shell chruby)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(robe-completing-read-func (quote ido-completing-read))
 '(rspec-autosave-buffer t)
 '(rspec-spec-command "rspec")
 '(rspec-use-bundler-when-possible nil)
 '(rspec-use-chruby t)
 '(rspec-use-docker-when-possible nil)
 '(rspec-use-opts-file-when-available nil)
 '(rspec-use-rake-when-possible nil)
 '(rspec-use-spring-when-possible t)
 '(rubocop-autocorrect-command "rubocop -a --format emacs")
 '(rubocop-autocorrect-on-save nil)
 '(rubocop-check-command "rubocop --format emacs")
 '(ruby-deep-arglist nil)
 '(ruby-deep-indent-paren nil)
 '(ruby-electric-newline-before-closing-bracket nil)
 '(ruby-flymake-use-rubocop-if-available t)
 '(setq nil t)
 '(web-mode-auto-close-style 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-closing t)
 '(web-mode-enable-auto-expanding t)
 '(web-mode-enable-auto-indentation t)
 '(web-mode-enable-auto-opening t)
 '(web-mode-enable-auto-pairing t)
 '(web-mode-enable-auto-quoting t)
 '(web-mode-enable-css-colorization t)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-enable-element-content-fontification t)
 '(web-mode-enable-element-tag-fontification t)
 '(web-mode-enable-engine-detection t)
 '(web-mode-enable-html-entities-fontification t)
 '(web-mode-enable-inlays t)
 '(web-mode-extra-auto-pairs nil)
 '(web-mode-markup-indent-offset 2)
 '(whitespace-action (quote (auto-cleanup))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
