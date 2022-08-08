;;; my-lispy.el --- describe
;;; Code:

;; (use-package smartparens
;;   :ensure t
;;   :hook
;;   (emacs-lisp-mode . smartparens-strict-mode)
;;   :config
;;   (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
;;   (sp-local-pair '(emacs-lisp-mode) "`" "`" :actions nil))

(use-package lispy
  :ensure t
  :hook ((clojure-mode emacs-lisp-mode) . lispy-mode)
  :config (lispy-set-key-theme '(lispy c-digits)))

(use-package lispyville
  :ensure
  :after (evil)
  :hook (lispy-mode . lispyville-mode)
  ;; :custom (lispyville-motions-put-into-special 't)
  :config
  (lispyville-set-key-theme
    '( operators
       C-w C-u
       (additional-movement normal visual motion))))


;; (use-package lispyville
;;   :ensure t
;;   :after (evil)
;;   :hook
;;   ;; ((emacs-lisp-mode clojure-mode) . lispyville-mode)
;;   ;; ((emacs-lisp-mode clojure-mode) . smartparens-strict-mode)
;;   (lispy-mode . lispyville-mode)
;;   ;; (emacs-lisp-mode . smartparens-strict-mode)
;;   :config
;;   ;; (evil-define-key '(normal visual motion) lispyville-mode-map
;;   ;;   "H" #'lispyville-backward-up-list
;;   ;;   "L" #'lispyville-up-list
;;   ;;   "." #'lispyville-forward-sexp
;;   ;;   "," #'lispyville-backward-sexp
;;   ;;   ")" #'lispyville-up-list
;;   ;;   "(" #'lispyville-backward-up-list
;;   ;;   "-" #'lispy-unwrap
;;   ;;   "_" #'lispy-wrap-round)
;;   (lispyville-set-key-theme
;;     '( ;; lispy
;;        ;; prettfy
;;        operators
;;        C-w
;;        ;; commentary
;;        ;; text-objects
;;        ;; slurp/barf-cp
;;        ;; (additional-motions normal visual motion)
;;        ;; (atom-movement t)
;;        )))

;; (evil-define-key '(normal visual) lispyville-mode-map
;;   "H" #'lispyville-backward-sexp
;;   "L" #'lispyville-forward-sexp
;;   ;; fixes
;;   "x" #'evil-delete-char
;;   "X" #'evil-delete-backward-char))

;; (kbd )") #'lispy-parens
;; completion search
;; up/down "form"
;; next/prev form on same level
;; lispyville move up/down
;; lispyville slurp/barf (</>?
;; self insert parens
;; major mode movement hydra instead of leader


;;; Commentary:
(provide 'my-lispy)
;;; my-lispy.el ends here
