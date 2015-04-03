;; Highlights matching parenthesis
(show-paren-mode 1)

;; Reload file when changed
(global-auto-revert-mode t)

;; Highlight current line
(global-hl-line-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; ido Mode
(ido-mode t)
;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; projectile
(projectile-global-mode)
;;(require 'helm-projectile)
;;(helm-projectile-on)

;; Enable VIM Emulation
;;(require 'evil)
;;(evil-mode 1)

;; Map left alt-Key with META and right with alt
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; Helm
(setq helm-command-prefix-key "C-c h")
(require 'helm-config)
(helm-mode 1)

;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)

;; when cursor is on edge, move to the other side, as in a toroidal space
(setq windmove-wrap-around t )

;; Kebindings
(global-set-key "\M-l" '(lambda () (interactive) (insert "@")))
(global-set-key "\M-5" '(lambda () (interactive) (insert "[")))
(global-set-key "\M-6" '(lambda () (interactive) (insert "]")))
(global-set-key "\M-7" '(lambda () (interactive) (insert "|")))
(global-set-key "\M-/" '(lambda () (interactive) (insert "\\")))
(global-set-key "\M-8" '(lambda () (interactive) (insert "{")))
(global-set-key "\M-9" '(lambda () (interactive) (insert "}")))
(global-set-key "\M-n" '(lambda () (interactive) (insert "~")))
