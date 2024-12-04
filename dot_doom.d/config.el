;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Travis B. Hartwell"
      user-mail-address "nafai@travishartwell.net")

;; TODO:
;; - Dynamically set font size based on screen resolution
;; - Set doom-variable-pitch-font and doom-big-font
(let ((tbh-font-family
       (seq-find #'doom-font-exists-p ["Monaspace Krypton" "SauceCodePro NF"])))
  (setq doom-font (font-spec :family tbh-font-family :size 16)))

(use-package! ef-themes
  :init
  (setq
   ef-themes-mixed-fonts t
   ef-themes-variable-pitch-ui t)
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-dark))

(setq doom-theme 'ef-dark)
(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))

(setq org-directory (expand-file-name "~/Documents/org/"))

(setq display-line-numbers-type t)

(setq ispell-dictionary "en")

(setq +format-on-save-enabled-modes
      '(not sh-mode))

;; Allows for editing files for use with https://github.com/booniepepper/dsg-md-posix/
(add-to-list 'auto-mode-alist '("\\.md\\.part\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.template\\'" . html-mode))

;; Use GNU grep on macOS for faster `consult-grep`
;; `doom doctor` is still complaining, because of hard-coded `grep` command
(after! consult
  (if (featurep :system 'macos)
      (setq consult-grep-args
            (append '("ggrep")
                    (cdr consult-grep-args)))))

;; Temporarily allow ~/.authinfo until I get gpg working again
(pushnew! auth-sources "~/.authinfo")

(defun tbh/ediff-init ()
  (interactive)
  (let ((default-directory doom-emacs-dir)
        (init-template-file "static/init.example.el"))
    (magit-ediff-compare tbh-last-init-el-template-commit "HEAD" init-template-file init-template-file)))

(use-package! lsp-treemacs-nerd-icons
  ;; HACK: Load after the `lsp-treemacs' created default themes
  :init (with-eval-after-load 'lsp-treemacs
          (require 'lsp-treemacs-nerd-icons)))

(use-package! lsp-treemacs
  :custom
  (lsp-treemacs-theme "nerd-icons-ext"))

(use-package! janet-mode
  :commands janet-mode
  :mode "\\.janet\\'"
  :interpreter "janet")

;; Proofread current file
(if (featurep :system 'macos)
    (defun tbh/proofread-file ()
      (interactive)
      (when
          (y-or-n-p "Save the file before proofreading?")
        (save-buffer))
      (goto-char (point-min))
      (let ((say-command-line (s-join " " `("say" "-f" ,(buffer-file-name)))))
        (start-process-shell-command "*proofreading with say*" nil say-command-line))))
