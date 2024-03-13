;;; tbh/chezmoi/autoload/company.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +chezmoi--company-backend-h ()
  (when (and chezmoi-mode
             (not (derived-mode-p 'emacs-lisp-mode)))
    (add-to-list 'company-backends 'chezmoi-company-backend)))
