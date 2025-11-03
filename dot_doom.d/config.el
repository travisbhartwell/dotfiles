;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Travis B. Hartwell"
      user-mail-address "nafai@travishartwell.net")

;; TODO:
;; - Dynamically set font size based on screen resolution
;; - Set doom-variable-pitch-font and doom-big-font
(let ((tbh-font-family
       (seq-find #'doom-font-exists-p ["Monaspace Krypton" "SauceCodePro NF"])))
  (setq doom-font (font-spec :family tbh-font-family :size 16)))

(if (featurep :system 'macos)
    (setq magit-git-executable "/opt/homebrew/bin/git"))

(use-package! ef-themes
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq
   ef-themes-mixed-fonts t
   ef-themes-variable-pitch-ui t)

  (mapc #'disable-theme custom-enabled-themes)
  (modus-themes-load-theme 'ef-dark))

(setq doom-theme 'ef-dark)
(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))

(setq org-directory (expand-file-name "~/Documents/org/"))

(setq display-line-numbers-type t)

(setq ispell-dictionary "en")

;; Configure formatting for sh-mode
(after! sh-script
  (set-formatter! 'shfmt-bash
    '("shfmt" "--case-indent" "--language-dialect" "bash" "--binary-next-line"
      (unless indent-tabs-mode
        (list "-i" (number-to-string tab-width))))
    :modes '((sh-mode (string= sh-shell "bash"))))

  (set-formatter! 'beautysh
    '("beautysh" "-"
      ("-i" "%d" (unless indent-tabs-mode tab-width)))
    :modes '((sh-mode (string= sh-shell "zsh")))))

(add-hook! 'sh-set-shell-hook
  (defun +sh-shell-set-formatter()
    (message "sh-shell set to '%s'" sh-shell)
    (message "+format-with set to '%s'" +format-with)

    (cond
     ((not (null +format-with))
      (message "Not overriding local set +format-with."))
     ((string= sh-shell "zsh")
      (progn
        (message "Setting formatter for zsh to beautysh")
        (setq +format-with 'beautysh)))
     ((string= sh-shell "bash")
      (progn
        (message "Setting formatter for bash to shfmt-bash")
        (setq +format-with 'shfmt-bash))))))

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

(defun tbh/ediff-init ()
  (interactive)
  (let ((default-directory doom-emacs-dir)
        (init-template-file "static/init.example.el"))
    (magit-ediff-compare tbh-last-init-el-template-commit "HEAD" init-template-file init-template-file)))

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

(map!
 :leader
 :desc "Run MyProject Task" "p m" #'tbh/mycmd-project-run-task-compilation)
