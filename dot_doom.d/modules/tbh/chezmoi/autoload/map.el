;;; tbh/chezmoi/autoload/map.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +chezmoi--init-h ()
  "Initialize chezmoi"
  (map! :leader
        (:prefix "f"
         :desc "Find file in dotfiles" "t" #'chezmoi-find
         :desc "Browse dotfiles" "T" #'+chezmoi--browse-dotfiles)))
