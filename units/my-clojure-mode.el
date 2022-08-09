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


;;; Commentary:
(provide 'my-clojure-mode)
;;; my-clojure-mode.el ends here
