;;; tbh/chezmoi/autoload/file-templates.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +chezmoi--file-templates-in-emacs-dirs-p-a (file)
  "Returns t if FILE is in Doom or your private directory."
  (or (file-in-directory-p file doom-user-dir)
      (file-in-directory-p file doom-emacs-dir)
      (file-in-directory-p file chezmoi-doom-private-dir)))

;;;###autoload
(defun +chezmoi--file-templates-get-short-path-a ()
  "Fetches a short file path for the header in Doom module templates."
  (let ((path (file-truename (or buffer-file-name default-directory))))
    (save-match-data
      (cond ((string-match "/modules/\\(.+\\)$" path)
             (match-string 1 path))
            ((file-in-directory-p path doom-emacs-dir)
             (file-relative-name path doom-emacs-dir))
            ((file-in-directory-p path doom-user-dir)
             (file-relative-name path doom-user-dir))
            ((file-in-directory-p path chezmoi-doom-private-dir)
             (file-relative-name path chezmoi-doom-private-dir))
            ((abbreviate-file-name path))))))
