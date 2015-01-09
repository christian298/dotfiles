;; Package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

;; Load and activate emacs packages.
;; Also sets the load path.
(package-initialize)

;; Define package list
(defvar my-packages '(
    better-defaults
    paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    smex
    exec-path-from-shell
    projectile
    rainbow-delimiters
    ))

(dolist (p my-packages)
    (when (not (package-installed-p p))
        (package-install p)))
