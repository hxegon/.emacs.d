;;; my-clojure-mode.el --- Clojure/Script support
;;; Code:

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
;; is this flycheck handling really necessary?

;; interactive clojure programming support
(use-package cider
  :ensure t
  :defer t
  :after clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  :config
  (setq
    ;; aggressive indent mode?
    cider-enrich-classpath nil
    cider-dynamic-indentation nil
    cider-prompt-for-symbol 't))
        ;; disable eldoc at point for cider in favor of lsp
        ;; cider-eldoc-display-for-symbol-at-point nil))

;; Hook clojure linter into flycheck
(use-package flycheck-clj-kondo
  :ensure t
  :after (flycheck))

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

;;; Commentary:
(provide 'my-clojure-mode)
;;; my-clojure-mode.el ends here
