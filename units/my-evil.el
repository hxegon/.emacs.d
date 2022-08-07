;;; my-evil.el --- describe
;;; Code:

(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search     ; better search mode than default
        evil-ex-complete-emacs-commands nil ; don't complete for emacs commands in ex-mode
        evil-want-keybinding nil            ; disable default evil bindings for non prog/text modes (needed for evil-collection)
        ;; evil-move-beyond-eol t ; helps with cleverparens
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

;;; Commentary:
(provide 'my-evil)
;;; my-evil.el ends here
