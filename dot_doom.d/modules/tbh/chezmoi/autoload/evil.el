;;; tbh/chezmoi/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +chezmoi--evil-insert-state-enter ()
  "Run after evil-insert-state-entry."
  (chezmoi-template-buffer-display nil (point))
  (remove-hook 'after-change-functions #'chezmoi-template--after-change 1))

;;;###autoload
(defun +chezmoi--evil-insert-state-exit ()
  "Run after evil-insert-state-exit."
  (chezmoi-template-buffer-display nil)
  (chezmoi-template-buffer-display t)
  (add-hook 'after-change-functions #'chezmoi-template--after-change nil 1))

;;;###autoload
(defun +chezmoi--evil-h ()
  (if chezmoi-mode
      (progn
        (add-hook 'evil-insert-state-entry-hook #'+chezmoi--evil-insert-state-enter nil 1)
        (add-hook 'evil-insert-state-exit-hook #'+chezmoi--evil-insert-state-exit nil 1))
    (progn
      (remove-hook 'evil-insert-state-entry-hook #'+chezmoi--evil-insert-state-enter 1)
      (remove-hook 'evil-insert-state-exit-hook #'+chezmoi--evil-insert-state-exit 1))))
