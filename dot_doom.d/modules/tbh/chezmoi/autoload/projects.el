;;; tbh/chezmoi/autoload/projects.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +chezmoi--browse-dotfiles ()
  "Browse the files in `chezmoi-dir'."
  (interactive)
  (doom-project-browse (expand-file-name chezmoi-dir)))
