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
    ("l" #'evil-switch-to-windows-last-buffer "Last buffer")
    ("SPC" #'ivy-switch-buffer "Switch buffer")
    ("B" #'ibuffer "ibuffer")
    ("k" #'kill-buffer "Kill this buffer" :color red)
    ("K" #'kill-current-buffer "Kill a buffer" :color red :exit t)
    ("s" #'save-buffer "Save buffer" :exit t)
    ("S" #'save-some-buffer "Save modified buffers" :exit t)
    ("r" #'revert-buffer "Revert buffer" :color red)
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

  (defhydra git-hydra
    (:color green :post (quit-windows-on "*git-gutter:diff*"))
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
    ;; Add "EVAL AT MARK" fn
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
    ;; add C-SPC and C-RET for opening in other window (or C-u prefix)
    ("SPC" #'counsel-projectile-find-file "Find in project")
    ("RET" #'bookmark-jump "Find in project")
    ("e" #'counsel-M-x "Execute Command")
    ("." #'counsel-find-file "Find File")
    ("f" #'file-hydra/body "Files")
    ("b" #'buffer-hydra/body "Buffer")
    ("w" #'window-hydra/body "Windows")
    ("g" #'git-hydra-if-repo "Git")
    ;; ("H" #'help-command "My help command")
    ;; sub-hydras
  )


  ;; add keybinding for hydra
  (evil-define-key `(normal visual) 'global (kbd "SPC") #'space-hydra/body)
  (evil-define-key `(normal visual) 'global (kbd ",") 'major-mode-hydra-launcher))


;;; Commentary:
(provide 'my-hydra)
;;; my-hydra.el ends here