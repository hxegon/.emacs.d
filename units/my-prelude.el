;;; my-prelude.el --- describe
;;; Code:

;; clojurey stdlib
(use-package dash)

;; cribbed from prelude
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
      (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;;; Commentary:
(provide 'my-prelude)
;;; my-prelude.el ends here
