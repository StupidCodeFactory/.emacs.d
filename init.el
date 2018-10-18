(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; (setq system-uses-terminfo nil)
(setq default-buffer-file-coding-system 'utf-8)
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
(setq ns-use-srgb-colorspace t)
(setq linum-format "%4d ")
(tool-bar-mode 0)
(electric-pair-mode t)
(require 'cl)
(toggle-frame-fullscreen)
(require 'ls-lisp)

(set-face-attribute 'default nil :height 180)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'chruby)

(require 'direnv)
(direnv-mode)
(setq direnv-always-show-summary t)
(unless (package-installed-p 'karma)
  (package-install 'karma))
(load-theme 'sanityinc-tomorrow-night t)

(global-git-gutter-mode +1)
(require 'textmate)
(textmate-mode t)

(smartscan-mode 1)
(global-linum-mode 1)

(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

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

(require 'rspec-mode)
(require 'ruby-mode)
(require 'bundler)
(require 'ruby-refactor)
(require 'flymake-ruby)
(require 'haml-mode)
(require 'ruby-block)
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
(setq ruby-bounce-deep-indent t)
(setq ruby-hanging-brace-indent-level 2)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'rspec-mode)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'ruby-block-mode)
(add-hook 'ruby-mode-hook 'ruby-block-highlight-toggle)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(setq ruby-deep-indent-paren nil)


(require 'projectile)
(projectile-mode +1)
(projectile-global-mode)
(projectile-rails-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(setq projectile-completion-system 'grizzl)

;; When folding, take these delimiters into consideration
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(class\\|def\\|do\\|if\\)" "\\(end\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))
(require 'feature-mode)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

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

(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")
      ))
(add-hook 'web-mode-hook
  (lambda ()
  (if (equal web-mode-content-type "javascript")
  (web-mode-set-content-type "jsx")
  (message "now set to: %s" web-mode-content-type))))

;; (require 'solidity-mode)
(require 'js2-mode)

(require 'mocha)

(require 'rg)
(rg-enable-default-bindings (kbd "M-s"))

(set-face-attribute 'default (selected-frame) :height 120)

(require 'ensime)
(require 'sbt-mode)
(require 'scala-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon-color "#f2777a")
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "bsd"))))
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(desktop-file-name-format (quote local))
 '(desktop-save t)
 '(desktop-save-mode t)
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
 '(global-whitespace-newline-mode nil)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (counsel ivy yaml-mode git-gutter ruby-tools ruby-electric robe hcl-mode terraform-mode ensime ripgrep mocha mocha-snippets company-solidity package-build epl git commander f dash go-mode emmet-mode color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow karma dockerfile-mode docker editorconfig ecukes cask-mode cask emamux dracula-theme nginx-mode direnv markdown-mode+ markdown-preview-mode flymd markdown-mode jsx-mode jdee ctags-update ctags magit grizzl bundler ag company ruby-dev ruby-block haml-mode fullframe flymake-ruby flx-ido feature-mode exec-path-from-shell chruby)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(robe-completing-read-func (quote ido-completing-read))
 '(rspec-autosave-buffer t)
 '(rspec-use-bundler-when-possible t)
 '(rspec-use-chruby t)
 '(rspec-use-docker-when-possible nil)
 '(rspec-use-opts-file-when-available nil)
 '(rspec-use-rake-when-possible nil)
 '(rspec-use-spring-when-possible nil)
 '(ruby-deep-arglist nil)
 '(ruby-deep-indent-paren nil)
 '(ruby-electric-newline-before-closing-bracket t)
 '(ruby-flymake-use-rubocop-if-available nil)
 '(session-use-package t nil (session))
 '(web-mode-auto-close-style 2)
 '(web-mode-enable-auto-expanding t)
 '(web-mode-enable-css-colorization nil)
 '(web-mode-enable-element-tag-fontification t)
 '(web-mode-enable-html-entities-fontification t)
 '(web-mode-enable-inlays t)
 '(web-mode-markup-indent-offset 2)
 '(whitespace-action (quote (auto-cleanup))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
