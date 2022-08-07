;;; init.el --- Cooper LeBrun

;;; -- initially cribbed from: https://github.com/suvratapte/dot-emacs-dot-d/blob/e7f44d981004b44bb9ae23b6b67c421404ea6b4e/init.el#L19 --
;;; Further inspo from https://github.com/Crandel/home/blob/master/.config/emacs/init.el

;;; Code:

;;; USE-PACKAGE

(require 'package)

;; add melpa to package archives.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; load and activate emacs packages. Done first to ensure packages are loaded before they are modified later. This also sets the load path
(package-initialize)

;; Install 'use-package' if it isn't already installed
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;; MY CONFIG UNITS

;; Add unit files to path
(add-to-list 'load-path (expand-file-name "units/" user-emacs-directory))

;;; package template:

;;; my-foo.el --- describe
;;; Code:
;;; Commentary:
;(provide 'my-foo)
;;; my-foo.el ends here

; (require 'saveplace)

;;; initial units
(require 'my-prelude) ; load utility functions / libs
(require 'base)
(require 'my-theme)
(require 'my-evil)
(require 'my-git)
(require 'my-which-key)
(require 'my-ivy)
(require 'my-projectile)
(require 'my-hydra)
(require 'my-clojure-mode)
(require 'my-lispy)

;; (require 'my-projectile)
;; (require 'my-yasnippet)

;; (require 'test-unit)
;; (load (expand-file-name "units/test-unit.el" user-emacs-directory))

;; Visual jump hints a la Easymotion
(use-package avy
  :ensure t)
  ;; :bind (("<backtab>" . avy-goto-line)
  ;;        ("C-<tab>" . avy-goto-char-timer)
  ;;        ("C-_" . avy-goto-line)
  ;;        ("M-_" . avy-goto-char-in-line)))

;; Package for markdown files, mostly used for syntax highlighting and org-mode style folding
;; (use-package markdown-mode
;;   :ensure t
;;   :defer t)

;; Automatic code linting
(use-package flycheck
  :ensure t
  :after (exec-path-from-shell)
  :init (global-flycheck-mode))

;; Magical auto completion
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-mode-minimum-prefix-length 1))

;; Magical emacs terminal finally (shamelessly cribbed from derek passen)
(use-package vterm
  :ensure t
  :defer t
  :bind (("C-x RET" . vterm-other-window)))

;; Better window navigation and swapping
(use-package ace-window
  :ensure t
  ; :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?h ?t ?n ?l ?a ?o ?e ?u))
  (ace-window-display-mode 1))

;; Resize windows automatically to intelligently utilize space
;; TODO: Add fn/binding to switch between golden ration and 50/50 ratios
(use-package zoom
  :ensure t
  :config
  (zoom-mode)
  (custom-set-variables
   '(zoom-size '(0.6 . 0.6))))

(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp))
  :config
  (lsp-enable-which-key-integration t)
  (setq
    gc-cons-threshold (* 100 1024 1024)
    read-process-output-max (* 1024 1024)
    treemacs-space-bewteen-root-nodes nil ; don't have treemacs
    lsp-lens-enable t
    lsp-signature-auto-activate nil))

;; (use-package lsp-ui-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

;; Used by spacemacs, idk.
;; (use-package evil-cleverparens
;;   :ensure t
;;   :after (evil hydra)
;;   :hook ((emacs-lisp-mode clojure-mode) . evil-smartparens-mode)
;;   :init
;;   (setq
;;     ;; Don't try to infer paren balancing when yanking. Toggle with M-T.
;;     evil-cleverparens-complete-parens-in-yanked-region nil
;;     evil-cleverparens-move-skip-delimiters nil)

;;   :config
;;   ;; force parenthesis balancing
;;   (smartparens-strict-mode)

;;   ;; don't try to pair single quotes in elisp
;;   (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)

;;   ;; ergo C-q binding. Used for inserting pair chars without cp handling it
;;   (evil-define-key 'insert 'evil-cleverparens-mode-map
;;     (kbd "C-'") #'evil-quoted-insert)

;;   (defhydra evil-cp-hydra
;;     (:color green)
;;     ("k" #'evil-cp-drag-backward)
;;     ("j" #'evil-cp-drag-forward)

;;     ("b" #'evil-cp-backward-sexp)
;;     ("B" #'evil-cp-backward-up-sexp)
;;     ("w" #'sp-next-sexp)
;;     ("W" #'evil-avy-goto-word-0)
;;     ("e" #'evil-cp-forward-sexp)
;;     ("E" #'paredit-forward-up)

;;     ("H" #'paredit-backword-down)
;;     ("L" #'paredit-forward-down)

;;     ("J" #'sp-join-sexp "join pairs []|[]->[|]" :exit t)
;;     ("X" #'sp-splice-sexp "splice pairs [[|]]->[|]" :exit t))
;;   ("r" #'sp-raise-sexp "kill all else and unwrap" :exit t)

;;   (evil-define-key '(normal visual) 'evil-cleverparens-mode-map
;;     "-" #'evil-cp-hydra/body
;;     ;; symbol
;;     ;; sexp forward
;;     ;; sexp backward
;;     (kbd "C-y") #'evil-cp-yank-enclosing)
;;   )
;; Load vcs excluded secrets elisp file if it exists
(let ((secrets "~/.emacs.d/secrets.el"))
  (when (file-exists-p secrets)
    (load-file secrets)))

;; https://www.reddit.com/r/emacs/comments/sn0xrd/screenwriting_with_fountainmode_and_olivettimode/
;; (use-package fountain-mode
;;   :ensure t)
;; (use-package olivetti
;;   :ensure t)

;; Org-roam mode for better note-taking (Graph style??? :)
;; https://jethrokuan.github.io/org-roam-guide/

;; C-s, query string, C-c C-o to open results in buffer, wgrep and edit all at once
;; Grab consul token for comcast from normal zsh env
(setenv "CONSUL_TOKEN" (shell-command-to-string ". ~/.zshrc; echo -n $CONSUL_TOKEN"))

;; ;; lispy doesn't execute commands correctly in normal mode, it'll act like point is before evil point rather then after
;; ;; so as if it's on the character before normal mode point like if it's in insert mode
;; ;; (use-package lispy
;; ;;   :ensure t)
;; (use-package lispy)
;; (use-package lispyville
;;   :ensure t
;;   :after (evil lispy)
;;   :hook ((emacs-lisp-mode clojure-mode) . lispyville-mode)
;;   :init
;;   :config
;;   (lispyville-set-key-theme
;;     '(lispy
;;        operators
;;        commentary
;;        text-objects
;;        slurp/barf-cp
;;        (atom-movement t)))
;;   (evil-define-key '(normal visual) lispyville-mode-map
;;     "H" #'lispyville-backward-sexp
;;     "L" #'lispyville-forward-sexp
;;     ;; fixes
;;     "x" #'evil-delete-char
;;     "X" #'evil-delete-backward-char))


;; (kbd )") #'lispy-parens
;; completion search
;; up/down "form"
;; next/prev form on same level
;; lispyville move up/down
;; lispyville slurp/barf (</>?
;; self insert parens
;; major mode movement hydra instead of leader

;; (lispy-mode)

(use-package minimap)
;; combine all 'switch buffer' ops to b hydra, bookmark jumping, file find, switch buffer, etc
