;;; init.el --- Cooper LeBrun

;;; Commentary:
;; fill this out

;;; -- initially cribbed from: https://github.com/suvratapte/dot-emacs-dot-d/blob/e7f44d981004b44bb9ae23b6b67c421404ea6b4e/init.el#L19 --

;;; Code:

;; -------- Prelude --------
(require 'package)

;; add melpa to package archives.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; load and activate emacs packages. Done first to ensure packages are loaded before they are modified later. This also sets the load path
(package-initialize)

;; Install 'use-package' if it isn't already installed
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;; Early packages

;; does this work?
(use-package no-littering ; keep emacs clean
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;;; initial emacs config

(setq-default
 ;; Don't show default startup screen.
 inhibit-startup-message t

 ;; Don't use compiled code if there's a newer package.
 load-prefer-newer t

 enable-recursive-minibuffers t

 ;; Do not put 'custom' config in init.el; put in another file.
 custom-file "~/.emacs.d/custom-file.el"

 ;; Don't create lockfiles
 create-lockfile nil

 ;; Don't use hard tabs
 indent-tabs-mode nil

 ;; Put backup files in their own folder: ~/.emacs.d/backups
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

 ;; Do not autosave
 auto-save-default nil

 ;; Replace obnoxious 'bell' sound with a visual indicator.
 visible-bell t

 ;; Try to keep 10 lines of context between cursor and end of window
 scroll-margin 10

 ;; Immediately select help window
 help-window-select t

 ;; Change elisp indentation offset from default of 4 -> 2
 lisp-indent-offset 2)

;; Load custom file
(load-file "~/.emacs.d/custom-file.el")

;; Change all yes/no questions to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Rebind M-o as 'other-window'
;(global-set-key (kbd "M-o") 'other-window)

;; Delete whitespace before a file is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable current line highlighting for programming and text modes
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(add-hook 'prog-mode-hook #'linum-mode)
(add-hook 'text-mode-hook #'linum-mode)

(tool-bar-mode -1)       ; disable gui emacs toolbar
(menu-bar-mode -1)       ; disable menu bar
(scroll-bar-mode -1)     ; disable gui scrollbar
(line-number-mode t)     ; add line numbers to status bar
(visual-line-mode t)     ; Toggles visual line mode: wraps lines and tweaks some commands accordingly

;; ----- ERGONOMICS -----

(global-set-key (kbd "M-o") 'other-window)

;; More vim like binding for for forward-paragraph and backward-paragraph
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; ergonomic join-line binding
(global-set-key (kbd "C-x j") 'join-line)

;; Binding for comment-region
(global-set-key (kbd "C-#") 'comment-region)

;; alternative to M-x
;;(global-set-key (kbd "C-<tab>") 'counsel-M-x)

;;; Fix for os x emacs not being able to set org timer sound b/c of dbus issue

;; Terminal notifier
;; requires 'brew install terminal-notifier'
;; stolen from erc-notifier

(add-hook 'prog-mode-hook 'show-paren-mode)

;;; -------- Packages --------

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-dracula))
  ;; :config (load-theme 'doom-flatwhite))
  ;; :config (load-theme 'doom-nord))
  ;; :config (load-theme 'doom-zenburn))

;; TODO how to get modeline to play nice with this theme
;; (use-package nano-theme
;;   :ensure t
;;   :config (nano-light))

;; Recent buffers
(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (add-to-list 'recentf-exclude (concat (getenv "HOME") "/.emacs.d/elpa/.*"))
  (recentf-mode t))

;; Better buffer management
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package diminish
  :ensure t)

;; Show guide for key combos
(use-package which-key
  :ensure t
  :config
  (which-key-mode t)
  :diminish which-key-mode)

;; Lightweight generic completion engine
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :demand t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'abbreviate
      uniquify-min-dir-content 10)
  (setq ivy-height 10)
  ;; Why did I add this? what does this do?
  (setq ivy-initial-inputs-alist nil)
  ;; Thought this was supposed to put a delay on ivy updating the results when typing but it doesn't.
  ;; Because it's the wrong thing or because it's broken?
  (setq ivy-dynamic-exhibit-delay-ms 350))

;; ivy functions replicating many common helm features
(use-package counsel
  :ensure t
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
         ("C-c i" . counsel-imenu)
         ("C-c f" . counsel-flycheck)
         ("C-x C-r" . counsel-recentf))
  :config
  (counsel-mode)
  :diminish counsel-mode)

;; isearch replacement using ivy
(use-package swiper
  :ensure t
  :demand t
  :bind (("C-s" . swiper)))

(use-package ivy-hydra
  :ensure t)

;; Smarter sorting algorithm for ivy
(use-package ivy-prescient
  :ensure t
  :after (ivy)
  :demand t
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode))

;; project based operations
(use-package projectile
  :ensure t
  :config (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :defer nil
  ;; :after (counsel projectile)
  :bind (("C-x /" . counsel-projectile-rg)
         ("C-x f" . counsel-projectile-find-file)
         ("C-x p" . counsel-projectile-switch-project)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Git integration
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package browse-at-remote
  :ensure t)

;; Show changed lines on the left
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode +1))

;; Visual jump hints a la Easymotion
(use-package avy
  :ensure t
  :bind (("<backtab>" . avy-goto-line)
         ("C-<tab>" . avy-goto-char-timer)
         ("C-_" . avy-goto-line)
         ("M-_" . avy-goto-char-in-line)))

;; -------- Clojure support --------
;; syntax highlighting for .clj, .cljs, .cljc
(use-package clojure-mode
  :ensure t
  :defer t
  :after (flycheck-clj-kondo)
  :mode
  (("\\.clj\\'" . clojure-mode)
   ("\\.cljs\\'" . clojurescript-mode)
   ("\\.edn\\'" . clojure-mode))
  :config
  (require 'flycheck-clj-kondo))

;; interactive clojure programming support
(use-package cider
  :ensure t
  :defer t
  :after clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  :config
  (setq
    cider-enrich-classpath nil
    cider-prompt-for-symbol 't))
        ;; disable eldoc at point for cider in favor of lsp
        ;; cider-eldoc-display-for-symbol-at-point nil))

;; Fixes path issues present when on using a gui emacs
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x)) ; Only run when using a graphical emacs
    (exec-path-from-shell-initialize)))

;; Package for markdown files, mostly used for syntax highlighting and org-mode style folding
;; (use-package markdown-mode
;;   :ensure t
;;   :defer t)

(use-package lua-mode
  :ensure t
  :defer t
  :mode (("\\.lua\\'" . lua-mode)))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.yml\\'" . yaml-mode)))

;; Automatic code linting
(use-package flycheck
  :ensure t
  :after (exec-path-from-shell)
  :init (global-flycheck-mode))

;; Hook clojure linter into flycheck
(use-package flycheck-clj-kondo
  :ensure t
  :after (flycheck))

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

;; Distributed collaboration using conflict-free replicated data types
(use-package crdt
  :ensure t)

;; Formats work clojure projects, needs manual triggering
(use-package zprint-format
  :ensure t
  :defer t)

;; Better window navigation and swapping
(use-package ace-window
  :ensure t
  ; :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?h ?t ?n ?l ?a ?o ?e ?u))
  (ace-window-display-mode 1))

;; Resize windows automatically to intelligently utilize space
;; TODO: Add fn/binding to switch between golden ration and 50/50 ratios
;; (use-package zoom
;;   :ensure t
;;   :config
;;   (zoom-mode)
;;   (custom-set-variables
;;    '(zoom-size '(0.618 . 0.618))))

(use-package csv-mode
  :defer t)

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

;; dired++
(use-package dirvish
  :ensure t
  :config (dirvish-override-dired-mode))

;; (use-package lsp-ui-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

;; HYDRA

(use-package hydra
  :ensure t
  :after (evil)
  :config

  ;; NAV HYDRA

  (defhydra navigation-hydra
    (:exit t)
    ("p" #'counsel-projectile-find-file "f/b in current project" :column "Project")
    ("P" #'counsel-projectile-switch-project "Switch project" :column "Project")
    ("r" #'counsel-recentf "recent files" :column "File")
    ("f" #'counsel-find-file "find file by path" :column "File")
    ("b" #'bookmark-jump "Jump to bookmark" :column "Bookmark")
    ("B" #'bookmark-set "Add bookmark" :column "Bookmark")
    ("d" #'dirvish "Dirvish" :column "Directory")
    )

  ;; BUFFER HYDRA

  (defun save-buffer-and-quit-hydra ()
    (interactive)
    (save-buffer)
    (hydra-keyboard-quit))

  (defhydra buffer-hydra
    (:color blue :exit t)
    ("b" #'ivy-switch-buffer "switch")
    ("l" #'evil-switch-to-windows-last-buffer "window's last buffer")
    ("B" #'ibuffer "ibuffer")
    ("k" #'kill-buffer "kill" :color red)
    ("x" #'kill-current-buffer "kill current" :color red :exit t)
    ("s" #'save-buffer-and-quit-hydra "save")
    ("R" #'revert-buffer "revert" :color red)
    )

  ;; WINDOW HYDRA

  (defhydra window-hydra
    (:color orange :exit t :hint none)
"
Windows:
^Jump^        ^Move^       ^Split^          ^Delete^
-----------------------------------------------------
_k_: up       _K_: up      _v_: vertical    _c_: this window
_j_: down     _J_: down    _s_: horizontal  _o_: other windows
_h_: left     _H_: left
_l_: right    _L_: right
_w_: ace      _R_: rotate
"
    ;; move
    ("H" #'evil-window-move-far-left)
    ("J" #'evil-window-move-far-down)
    ("K" #'evil-window-move-far-up)
    ("L" #'evil-window-move-far-right)
    ("R" #'evil-window-rotate-downwards)

    ;; jump
    ("h" #'evil-window-left)
    ("j" #'evil-window-down)
    ("k" #'evil-window-up)
    ("l" #'evil-window-right)
    ("w" #'ace-window)

    ;; delete
    ("c" #'delete-window)
    ("o" #'delete-other-windows)

    ;; split
    ("s" #'evil-window-split)
    ("v" #'evil-window-vsplit)
    )

  ;; GIT HYDRA

  (defun browse-repo ()
    (interactive)
    (-> (projectile-acquire-root)
        browse-at-remote--file-url
        browse-url))

  (defhydra git-hydra
    (:color green :post (quit-windows-on "*git-gutter:diff*"))
    "Git"

    ("g" #'magit-status "Status" :exit t :column "Repo")
    ("F" #'magit-pull "Pull" :column "Repo" :exit t)
    ("c" #'magit-commit "Commit" :column "Repo")
    ("P" #'magit-push "Push" :column "Repo" :exit t)
    ("B" #'browse-repo "open in github" :column "Repo" :exit t)

    ("U" #'magit-unstage-all "unstage all files" :column "File" :exit t)
    ("S" #'magit-stage-all "stage all files" :column "File" :exit t)
    ("f" #'magit-stage-file "stage file" :column "File")
    ("b" #'browse-at-remote "open in github" :column "File" :exit t)

    ("n" #'git-gutter:next-hunk "Jump to next" :column "Hunk")
    ("p" #'git-gutter:previous-hunk "Jump to previous" :column "Hunk")
    ("s" #'git-gutter:stage-hunk "stage" :column "Hunk")
    ("u" #'git-gutter:unstage-hunk "unstage" :column "Hunk")
    ("d" #'git-gutter:popup-diff "diff" :column "Hunk")
    ("x" #'git-gutter:revert-hunk "revert" :column "Hunk")

    ("G" #'git-gutter:update-all-windows "update git gutters" :column "Info")
    ("l" #'git-gutter:statistic "line stats" :column "Info")
    ("a" #'vc-annotate "annotate lines" :column "Info" :exit t)

    ("q" #'hydra-keyboard-quit "exit hydra" :exit t)
    )

  (defun git-hydra-if-repo ()
    (interactive)
    (if (vc-root-dir)
      (git-hydra/body)
      (message "No git repo detected! Aborting magit-hydra!")))

  ;; MAJOR MODE HYDRA

  (defhydra emacs-lisp-mode-hydra
    (:exit t)
    ("e" #'eval-defun "eval defun" :column "Eval")
    ("x" #'eval-last-sexp "eval last sexp" :column "Eval")
    ("b" #'eval-buffer "eval buffer" :column "Eval")

    ("v" #'counsel-describe-variable "describe variable" :column "Describe")
    ("f" #'counsel-describe-function "describe function" :column "Describe")
    ("m" #'counsel-describe-mode "describe mode" :column "Describe")
    )

  (defhydra clojure-mode-hydra
    (:exit t)
    ("e" #'cider-eval-defun-at-point "eval defun (C-u to inst.)" :column "Eval")
    ("x" #'cider-eval-last-sexp "eval last sexp" :column "Eval")
    ("b" #'cider-eval-buffer "eval buffer" :column "Eval")
    ("n" #'cider-ns-reload "reload ns" :column "Eval")
    ("N" #'cider-ns-reload-all "reload ns and libs" :column "Eval")

    ("j" #'cider-jack-in-clj "jack in clj" :column "REPL")
    ("J" #'cider-jack-in-cljs "jack in cljs" :column "REPL")
    ("c" #'cider-connect "connect" :column "REPL")
    ("r" #'cider-switch-to-repl-buffer "repl" :column "REPL")
    ("q" #'cider-quit "quit" :column "REPL")

    ("d" #'cider-doc "cider doc" :column "Docs")
    ("D" #'cider-clojuredocs "clojuredocs" :column "Docs")
    ("w" #'cider-clojuredocs-web "clojuredocs in browser" :column "Docs")
    )

  (defun major-mode-hydra-launcher ()
    (interactive)
    (cl-case major-mode
      ('emacs-lisp-mode (emacs-lisp-mode-hydra/body))
      ('clojure-mode (clojure-mode-hydra/body))
      ('clojurescript-mode (clojure-mode-hydra/body))
      ('cider-repl-mode (clojure-mode-hydra/body)) ;; maybe make a specific cider repl hydra
      (t (message "No major mode hydra found"))
      )
    )

  ;; SPACE HYDRA

  ;; Top level hydra @ spacebar in normal and visual modes
  (defhydra space-hydra
    (:color red :exit t)
    ("SPC" #'counsel-M-x "M-x")
    ("n" #'navigation-hydra/body "Navigation")
    ("b" #'buffer-hydra/body "Buffer")
    ("w" #'window-hydra/body "Windows")
    ("g" #'git-hydra-if-repo "Magit")
    ;; ("H" #'help-command "My help command")
    ;; sub-hydras
  )
  ;; add keybinding for hydra
  (evil-define-key `(normal visual) 'global (kbd "SPC") #'space-hydra/body)
  (evil-define-key `(normal visual) 'global (kbd ",") 'major-mode-hydra-launcher))

;;; --- EVIL ---

(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search     ; better search mode than default
        evil-ex-complete-emacs-commands nil ; don't complete for emacs commands in ex-mode
        evil-want-keybinding nil            ; disable default evil bindings for non prog/text modes (needed for evil-collection)
        ;; v dwis. Temporary measure until my evil-stuff is more polished
        ;; evil-disable-insert-state-bindings t)
        )
  :config
  (evil-mode 1)
  (evil-set-leader `(normal visual) (kbd "-"))
  (evil-define-key `(normal visual) 'global
    "Y" "y$"                                 ; Capital Y yanks to end of line
    "Q" "@q"                                 ; Q triggers macro in q register
    "L"                'evil-last-non-blank  ; faster beginning of line navigation
    "H"                'evil-first-non-blank ; faster end of line navigation
    (kbd "C-y")        'evil-paste-before    ; mimic default emacs yank binding (useful for putting in swiper)
    (kbd "C-r")        'counsel-buffer-or-recentf    ; mimic default emacs yank binding (useful for putting in swiper)

    (kbd "<leader>t")  'avy-goto-char-timer  ; char jump on timer
    (kbd "<leader>l")  'avy-goto-line        ; jump to line
    (kbd "<leader>-")  'avy-goto-symbol-1    ; 1 char symbol jump
    (kbd "<leader>.")  'avy-goto-char        ; 1 char jump
    (kbd "<leader>o")  'ace-window        ; 1 char jump
    (kbd "<leader>p")  'counsel-yank-pop     ; paste from a search menu of recent kills

    (kbd "<leader><tab>") 'minimap-mode

    ;; -> context
    (kbd "<leader>F")  'counsel-flycheck     ; show errors/warnings in a search minibuffer

    ;; -> formatting hydra
    (kbd "<leader>A")  'align-regexp         ; align region by regular expresion
    (kbd "<leader>/")  'evil-ex-nohighlight  ; clear highlighting from / searches
  )
  ;; Better visual indication of mode (at where I'm actually looking when editing)
  (setq evil-insert-state-cursor '((bar . 2) "red")
        evil-normal-state-cursor '(box "green")
        evil-visual-state-cursor '(box "blue")))

;; cs"' <- change surrounding double quotes to single quotes.
(use-package evil-surround
  :ensure t
  :after (evil)
  :config (global-evil-surround-mode 1))

;; f/t highlight unique letters in line for faster line-wise jumping
(use-package evil-quickscope
  :ensure t
  :after (evil)
  :config (global-evil-quickscope-always-mode 1))

(use-package evil-commentary
  :ensure t
  :after (evil)
  :config (evil-commentary-mode))

(use-package evil-collection
  :ensure t
  :after (evil)
  :config
  (evil-collection-init))

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
(use-package wgrep
  :ensure t
  :config (setq wgrep-change-readonly-file t))

;; Grab consul token for comcast from normal zsh env
(setenv "CONSUL_TOKEN" (shell-command-to-string ". ~/.zshrc; echo -n $CONSUL_TOKEN"))

(use-package minimap)
