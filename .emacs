(set-language-environment "UTF-8")

;; ----------------------------------------------------------------------
;; set color theme
;; ----------------------------------------------------------------------
;;(add-to-list 'load-path "/home/sober/.emacs.d/elpa/color-theme-6.6.0")
;;(require 'color-theme)
;;(color-theme-charcoal-black)

;; ----------------------------------------------------------------------
;; groovy-mode
;; ----------------------------------------------------------------------
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at
;;; start
(add-to-list 'load-path "/home/sober/.emacs.d/elpa/groovy-mode")
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
                          (groovy-electric-mode)))

;; ----------------------------------------------------------------------
;; ido-mode for buffer selection
;; ----------------------------------------------------------------------
(ido-mode t)

;; ----------------------------------------------------------------------
;; SLIME section
;; Load and configure SLIME
;; ----------------------------------------------------------------------
(add-to-list 'load-path "/home/data/dev/bin/clojure/slime")

(require 'slime)
(eval-after-load 'slime '(setq slime-protocol-version 'ignore))
(slime-setup '(slime-repl))
(put 'upcase-region 'disabled nil)

(require 'cl)
(setq-default save-place t)
(global-set-key (kbd "C-c f") 'find-file-in-project)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror"
      marmalade-server "http://marmalade-repo.org/"
      custom-file (expand-file-name "~/.emacs.d/custom.el")
      ispell-extra-args '("--keyboard=dvorak")
      ido-handle-duplicate-virtual-buffers 2)

;; ----------------------------------------------------------------------
;; Package management
;; ----------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
                                  starter-kit-eshell scpaste
                                  clojure-mode clojure-test-mode
                                  markdown-mode yaml-mode tuareg
                                  marmalade oddmuse scpaste))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; ----------------------------------------------------------------------
;; find file in TAGS file
;; ----------------------------------------------------------------------
(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(global-set-key (kbd "C-c M-.") 'ido-find-file-in-tag-files)

;; ----------------------------------------------------------------------
;; auto-complete-mode
;; ----------------------------------------------------------------------
(require 'auto-complete-config)
(ac-config-default)
(require 'auto-complete-etags)

;; ----------------------------------------------------------------------
;; whitespace mode
;; ----------------------------------------------------------------------
(require 'whitespace)
(setq whitespace-style '(trailing tabs tab-mark
                                  indentation
                                  space-before-tab
                                  space-after-tab))

;; ----------------------------------------------------------------------
;; stop the war between tabs and spaces
;; http://www.emacswiki.org/emacs/FuzzyFormat
;; ----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elpa/fuzzy-format/")
(require 'fuzzy-format)
(setq fuzzy-format-default-indent-tabs-mode nil)
(global-fuzzy-format-mode t)

;; ----------------------------------------------------------------------
;; configure tab behaviour
;; ----------------------------------------------------------------------
;;(global-set-key "\C-i" 'self-insert-command)
;;(setq indent-line-function 'insert-tab)
(setq tab-width 4)
(setq default-tab-width 4)
;;(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
(setq indent-tabs-mode nil)

;; highlight current line
(global-hl-line-mode 1)

;; ----------------------------------------------------------------------
;; start eshel ... why not?
;; ----------------------------------------------------------------------
(eshell)
;; graaaaaaah! eshell doesn't respect eval-after-load for some reason:
(with-current-buffer "*eshell*" (setq pcomplete-cycle-completions nil))
(set-face-foreground 'eshell-prompt "turquoise")
(put 'ido-exit-minibuffer 'disabled nil)
