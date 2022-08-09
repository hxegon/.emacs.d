;;; my-hydra.el --- describe
;;; Code:

(use-package hydra
  :ensure t
  :after (evil)
  :config

  ;; NAV HYDRA

  (defhydra file-hydra
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

  (defhydra buffer-hydra
    ;; Change next/previous buffer to not exit. Could also display the "buffer order or whatever"
    (:color blue :exit t :columns 3)
    ("p" #'previous-buffer "Previous buffer")
    ("n" #'next-buffer "Next buffer")
    ("l" #'evil-switch-to-windows-last-buffer "Last buffer" :exit t)
    ("b" #'ivy-switch-buffer "Switch buffer" :exit t)
    ("B" #'ibuffer "ibuffer" :exit t)
    ("K" #'kill-buffer "Kill buffer" :color red :exit t)
    ("k" #'kill-current-buffer "Kill this buffer" :color red :exit t)
    ("s" #'save-buffer "Save buffer" :exit t)
    ("S" #'save-some-buffer "Save modified buffers" :exit t)
    ("r" #'revert-buffer "Revert buffer" :color red))

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
    ("H" #'evil-window-move-far-left :exit t)
    ("J" #'evil-window-move-far-down :exit t)
    ("K" #'evil-window-move-far-up :exit t)
    ("L" #'evil-window-move-far-right :exit t)
    ("R" #'evil-window-rotate-downwards :exit t)

    ;; jump
    ("h" #'evil-window-left)
    ("j" #'evil-window-down)
    ("k" #'evil-window-up)
    ("l" #'evil-window-right)
    ("w" #'ace-window :exit t)

    ;; delete
    ("c" #'delete-window :exit t)
    ("o" #'delete-other-windows :exit t)

    ;; split
    ("s" #'evil-window-split :exit t)
    ("v" #'evil-window-vsplit :exit t))


  ;; GIT HYDRA

  (defhydra git-hydra
    ;; (:color green :post (quit-windows-on "*git-gutter:diff*"))
    (:color green)
    "Git"

    ("g" #'magit-status "Status" :exit t :column "Repo")
    ("F" #'magit-pull "Pull" :column "Repo" :exit t)
    ("c" #'magit-commit "Commit" :column "Repo")
    ("P" #'magit-push "Push" :column "Repo" :exit t)
    ("B" #'browse-repo "open in github" :column "Repo" :exit t)

    ("U" #'magit-unstage-all "unstage all files" :column "File" :exit t)
    ;; ("S" #'magit-stage-all "stage all files" :column "File" :exit t)
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

  (defhydra elisp-eval-hydra
    (:exit t)
    ("e" #'eval-defun "eval defun")
    ("x" #'eval-last-sexp "eval last sexp")
    ("b" #'eval-buffer "eval buffer"))

  (defhydra elisp-doc-hydra
    (:exit t)
    ("v" #'counsel-describe-variable "describe variable")
    ("f" #'counsel-describe-function "describe function")
    ("m" #'counsel-describe-mode "describe mode"))

  (defhydra emacs-lisp-mode-hydra
    (:exit t)
    ("d" #'elisp-doc-hydra/body "Documentation")
    ("e" #'elisp-eval-hydra/body "Eval"))

  (defhydra clojure-eval-hydra
    (:exit t)
    ("e" #'cider-eval-defun-at-point "eval defun (C-u to inst.)" :column "Eval")
    ("l" #'cider-eval-last-sexp "eval last sexp" :column "Eval")
    ("b" #'cider-eval-buffer "eval buffer" :column "Eval")
    ("n" #'cider-ns-reload "reload ns" :column "Eval")
    ("N" #'cider-ns-reload-all "reload ns and libs" :column "Eval"))

  (defhydra clojure-repl-hydra
    (:exit t)
    ("j" #'cider-jack-in-clj "jack in clj")
    ("J" #'cider-jack-in-cljs "jack in cljs")
    ("c" #'cider-connect "connect")
    ("r" #'cider-switch-to-repl-buffer "switch to repl buffer")
    ("k" #'cider-quit "kill cider"))

  (defhydra clojure-doc-hydra
    (:exit t)
    ("c" #'cider-doc "cider doc" :column "Docs")
    ("d" #'cider-clojuredocs "clojuredocs" :column "Docs")
    ("w" #'cider-clojuredocs-web "clojuredocs in browser" :column "Docs"))

  (defhydra clojure-goto-hydra
    (:exit t)
    ("d" #'evil-goto-definition "Goto definition"))

  (defhydra clojure-mode-hydra
    ;; Add "EVAL AT MARK" fn
    (:exit t)
    ("e" #'clojure-eval-hydra/body "Eval")
    ("d" #'clojure-doc-hydra/body "Documentation")
    ("r" #'clojure-repl-hydra/body "REPL")
    ("s" #'cider-scratch "Scratch buffer")
    ("g" #'clojure-goto-hydra/body "GoTo"))

  (defun major-mode-hydra-launcher ()
    (interactive)
    (cl-case major-mode
      ('emacs-lisp-mode (emacs-lisp-mode-hydra/body))
      ('clojure-mode (clojure-mode-hydra/body))
      ('clojurec-mode (clojure-mode-hydra/body))
      ('clojurescript-mode (clojure-mode-hydra/body))
      ('cider-repl-mode (clojure-mode-hydra/body)) ;; maybe make a specific cider repl hydra
      (t (message "No major mode hydra found"))))

  (defun jump-to-init ()
    (interactive)
    (find-file (expand-file-name "~/.emacs.d/init.el")))

  (defhydra config-hydra
    (:exit t)
    ( "i" #'jump-to-init "Open init.el")
    ("SPC" #'open-init-and-find-project-file "Find config file"))

  ;; SPACE HYDRA

  ;; Top level hydra @ spacebar in normal and visual modes
  (defhydra space-hydra
    (:color red :exit t)
    ;; add C-SPC and C-RET for opening in other window (or C-u prefix)
    ("SPC" #'counsel-projectile-find-file "Find in project")
    ("RET" #'bookmark-jump "Find in project")
    ("e" #'counsel-M-x "Execute Command")
    ("." #'counsel-find-file "Find File")
    ("f" #'file-hydra/body "Files")
    ("b" #'buffer-hydra/body "Buffer")
    ("w" #'window-hydra/body "Windows")
    ("g" #'git-hydra-if-repo "Git")
    ("c" #'major-mode-hydra-launcher "Code")
    ("C" #'config-hydra/body "Config"))
  ;; ("H" #'help-command "My help command")
  ;; sub-hydras

  ;; add keybinding for hydra
  (evil-define-key `(normal visual) 'global (kbd "SPC") #'space-hydra/body))

;;; Commentary:
(provide 'my-hydra)
;;; my-hydra.el ends here
