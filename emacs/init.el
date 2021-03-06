;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; ----------------------------------------------------------------------------
;;; General Emacs Configuration
;;; ----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Melpa Configuration
;; -----------------------------------------------------------------------------
;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; -----------------------------------------------------------------------------
;; Enabling advanced commands
;; -----------------------------------------------------------------------------

(put 'narrow-to-region 'disabled nil) ; Enable narrowing

;; -----------------------------------------------------------------------------
;; Org-mode configuration
;; -----------------------------------------------------------------------------

(require 'org)
(setq org-log-done t) ; Append date to TODO item when closed
(setq org-latex-packages-alist
      '(("" "bussproofs" t)))
      
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "DONE")))
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.5)) ; Latex preview size

;; -----------------------------------------------------------------------------
;; Color Theme
;; -----------------------------------------------------------------------------
(unless (package-installed-p 'zenburn-theme)
  (package-refresh-contents)
  (package-install 'zenburn-theme))

(load-theme 'zenburn t)

;; -----------------------------------------------------------------------------
;; Save newlines at end of files
;; -----------------------------------------------------------------------------

(setq require-final-newline 'visit-save)

;; -----------------------------------------------------------------------------
;; Show trailing whitespace
;; -----------------------------------------------------------------------------
(setq-default show-trailing-whitespace t)

;; -----------------------------------------------------------------------------
;; Disable Backup files (*~ and *#). I version all files with git.
;; -----------------------------------------------------------------------------

(setq make-backup-files nil) ; No *~ files.
(setq auto-save-default nil) ; No *# files.

;; -----------------------------------------------------------------------------
;; Line Numbering
;; -----------------------------------------------------------------------------

(global-linum-mode t)
(setq line-number-mode nil)
(setq column-number-mode t)

;; -----------------------------------------------------------------------------
;; Window Decorations
;; -----------------------------------------------------------------------------

(tool-bar-mode -1)    ; Disable Toolbar
(scroll-bar-mode -1)  ; Disable Scrollbar

;; -----------------------------------------------------------------------------
;; Emacs startup
;; -----------------------------------------------------------------------------

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; -----------------------------------------------------------------------------
;; IDO Mode (Interactively Do Things
;; -----------------------------------------------------------------------------

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; -----------------------------------------------------------------------------
;; Minibuffer Options
;; -----------------------------------------------------------------------------

(set 'echo-keystrokes -1) ; Echo keystrokes without 1s delay.

;; -----------------------------------------------------------------------------
;; Neotree Configuration (Browsing directories)
;; -----------------------------------------------------------------------------

(unless (package-installed-p 'neotree)
  (package-refresh-contents)
  (package-install 'neotree))

(require 'neotree)
(global-set-key [f8] 'neotree-toggle) ; With <F8> toggle the file browser
(setq neo-theme 'ascii)               ; Set style of the tree to ascii signs

;; -----------------------------------------------------------------------------
;; Magit Configuration
;; -----------------------------------------------------------------------------

(global-set-key (kbd "C-x g") 'magit-status)

;; -----------------------------------------------------------------------------
;; Resizing Frames
;; -----------------------------------------------------------------------------

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally) ; Control-shift left
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; -----------------------------------------------------------------------------
;; Font settings
;; -----------------------------------------------------------------------------

(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-9"))

;; -----------------------------------------------------------------------------
;; Flycheck Config
;; -----------------------------------------------------------------------------
(unless (package-installed-p 'flycheck)
  (package-refresh-contents)
  (package-install 'flycheck))

(require 'flycheck)
(global-flycheck-mode)                     ; Global Flycheck
(setq flycheck-check-syntax-automatically
      '(mode-enabled idle-change save))    ; Rerun check on idle and save

;; -----------------------------------------------------------------------------
;; Config from emacs' Custom system
;; -----------------------------------------------------------------------------

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;;; ----------------------------------------------------------------------------
;;; Configuration for specific languages
;;; ----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Language Server Protocol (LSP)
;; -----------------------------------------------------------------------------

(unless (package-installed-p 'lsp-ui)
  (package-refresh-contents)
  (package-install 'lsp-ui))
(unless (package-installed-p 'lsp-haskell)
  (package-refresh-contents)
  (package-install 'lsp-haskell))
(unless (package-installed-p 'company-lsp)
  (package-refresh-contents)
  (package-install 'company-lsp))

(require 'lsp-mode)
(require 'lsp)
(require 'lsp-ui)
(require 'lsp-haskell)
(require 'company-lsp)
(push 'company-lsp company-backends)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq lsp-enable-snippet nil)

;; -----------------------------------------------------------------------------
;; Yaml-Mode
;; -----------------------------------------------------------------------------

(unless (package-installed-p 'yaml-mode)
  (package-refresh-contents)
  (package-install 'yaml-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist
	     '("\\.yml\\'" . yaml-mode))
	     
;; -----------------------------------------------------------------------------
;; Haskell
;; -----------------------------------------------------------------------------

(require 'lsp-haskell)
(require 'haskell-interactive-mode)
(add-hook 'haskell-mode-hook
	  #'lsp
	  'interactive-haskell-mode)

;; Haskell Mode

;; (require 'haskell-mode)
;; (require 'haskell-interactive-mode)
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; ;; Liquid Haskell

;; (add-to-list 'load-path (expand-file-name "liquid-tip.el/" user-emacs-directory))

;; Configure flycheck-liquidhs, if you haven't already
;(add-hook 'haskell-mode-hook
;          '(lambda () (flycheck-select-checker 'haskell-liquid)))

;(add-hook 'literate-haskell-mode-hook
;          '(lambda () (flycheck-select-checker 'haskell-liquid)))

;(require 'liquid-types)

;; Toggle minor mode on entering Haskell mode.
;(add-hook 'haskell-mode-hook
;          '(lambda () (liquid-types-mode)))
;(add-hook 'literate-haskell-mode-hook
;	  '(lambda () (liquid-types-mode)))


;;----------------------------------
;; OCaml Mode
;;----------------------------------
;; Tuareg
;; (load "~/ocaml/tuareg/tuareg-site-file")
;; (dolist
;;    (var (car (read-from-string
;; 	      (shell-command-to-string "opam config env --sexp"))))
;;   ;; Merlin
;;   (setenv (car var) (cadr var)))
;; (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;;   (when (and opam-share (file-directory-p opam-share))                                                                                             
;;     ;; Register Merlin
;;     (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;     (autoload 'merlin-mode "merlin" nil t nil)
;;     ;; Automatically start it in OCaml buffers
;;     (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;     (add-hook 'caml-mode-hook 'merlin-mode t)
;;     ;; Use opam switch to lookup ocamlmerlin binary
;;     (setq merlin-command 'opam)))

;; -----------------------------------------------------------------------------
;; Idris
;; -----------------------------------------------------------------------------

;; (require 'idris-mode)

;; -----------------------------------------------------------------------------
;; Lean
;; -----------------------------------------------------------------------------

;(defvar lean-mode-required-packages
;  '(company dash dash-functional flycheck f
;            fill-column-indicator s))
;(let ((need-to-refresh t))
;  (dolist (p lean-mode-required-packages)
;    (when (not (package-installed-p p))
;      (when need-to-refresh
;        (package-refresh-contents)
;        (setq need-to-refresh nil))
;      (package-install p))))
;; Set up lean-root path
;(setq lean-rootdir "~/Lean/lean2-master")  ;; <=== YOU NEED TO MODIFY THIS
;(setq-local lean-emacs-path
;            (concat (file-name-as-directory lean-rootdir)
;                    (file-name-as-directory "src")
;                    "emacs"))
;(add-to-list 'load-path (expand-file-name lean-emacs-path))
;(require 'lean-mode)

;; -----------------------------------------------------------------------------
;; Coq
;; -----------------------------------------------------------------------------

(unless (package-installed-p 'proof-general)
  (package-refresh-contents)
  (package-install 'proof-general))

;; Proof General
(require 'proof-site)
(proof-ready-for-assistant 'coq)

(setq proof-splash-enable nil)
(setq proof-electric-terminator-enable nil)
(setq proof-next-command-insert-space nil)
(setq PA-one-command-per-line nil)

;; Company Coq

(add-hook 'coq-mode-hook #'company-coq-mode)
(add-hook 'coq-mode-hook
          (lambda ()
            (setq-local prettify-symbols-alist
                        '(
			  ("Qed." . ?■)
                          ("Defined." . ?□)
			  ))))


;; -----------------------------------------------------------------------------
;; Agda
;; -----------------------------------------------------------------------------

;; (load-file (let ((coding-system-for-read 'utf-8))
;; 	     (shell-command-to-string "agda-mode locate")))

;; -----------------------------------------------------------------------------
;; Elisp
;; -----------------------------------------------------------------------------

;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)

;; -----------------------------------------------------------------------------
;; Cubical Type Theory (CTT)
;; -----------------------------------------------------------------------------

;; (load-file "~/GitRepos/cubicaltt/cubicaltt.el")
;; (autoload 'cubicaltt-mode "cubicaltt" "cubical editing mode" t)
;; (setq auto-mode-alist (append auto-mode-alist '(("\\.ctt$" . cubicaltt-mode))))

;; -----------------------------------------------------------------------------
;; Uroboro
;; -----------------------------------------------------------------------------

; (load "~/GitRepos/gacodt/CoqHaskell/uroboro.el")
; (require 'uroboro-mode)
