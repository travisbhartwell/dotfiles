;;; tbh/notes/config.el -*- lexical-binding: t; -*-

(use-package! denote
  :commands
  denote
  :init
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  :config
  (setq denote-directory (expand-file-name "~/Documents/Notes")
        denote-known-keywords '("mycmd" "productivity")
        denote-infer-keywords t
        denote-sort-keywords t))
