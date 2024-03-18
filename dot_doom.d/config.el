;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'ef-dark)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (expand-file-name "~/Documents/org/"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq ledger-post-amount-alignment-column 68)

(setq ispell-dictionary "en")

(setq +format-on-save-enabled-modes
      '(not sh-mode))

(defconst tbh/mycmd-executable
  (executable-find "mycmd"))

(defun tbh/mycmd-direct-path (&rest parts)
  (f-expand (apply #'f-join "~/mycmd" parts)))

;; (defun tbh/mycmd-call (&rest args)
;;   (apply #'doom-call-process (cons tbh/mycmd-executable (remq nil args))))

(defun tbh/mycmd-call (cmd &rest args)
  (apply #'doom-call-process (cons cmd (remq nil args))))

;; (defun tbh/get-source-csvs (account)
;;   (-let*
;;       (((status . output) (tbh/mycmd-call "financial" "list-source-csvs"))
;;        (output-lines (-filter (lambda (line) (s-starts-with? account line)) (s-lines output))))
;;     (-map (lambda (line) (-drop 1 (s-split " " line))) output-lines)))

(defun tbh/get-source-csvs (account)
  (-let*
      ((list-csvs-cmd (tbh/mycmd-direct-path "financial" "list-source-csvs"))
       ((status . output) (tbh/mycmd-call list-csvs-cmd))
       (output-lines (-filter (lambda (line) (s-starts-with? account line)) (s-lines output))))
    (-map (lambda (line) (-drop 1 (s-split " " line))) output-lines)))

(defun tbh/ledger-next-imported-xact ()
  (interactive)
  (search-forward "; SOURCE")
  (ledger-navigate-beginning-of-xact))

;; (defun tbh/import-csv (account import-command)
;;   (interactive)
;;   (-let*
;;       ((import-date (completing-read "Select date to import: " (tbh/get-source-csvs account) nil t nil nil))
;;        ((status . output) (tbh/mycmd-call "financial" "import" import-command import-date))
;;        (generated-file (-second-item (s-match "\\- Wrote .* Ledger entries to file \\(.*\\).$" output))))
;;     (message "Importing ledger postings from %s." generated-file)
;;     (goto-char (point-max))
;;     (newline)
;;     (insert-file-contents generated-file)
;;     (+format-buffer-h)
;;     (goto-char (point-min))
;;     (tbh/ledger-next-imported-xact)))

(defun tbh/import-csv (account import-command)
  (interactive)
  (-let*
      ((import-date (completing-read "Select date to import: " (tbh/get-source-csvs account) nil t nil nil))
       (import-cmd-path (tbh/mycmd-direct-path "financial" "import" import-command))
       ((status . output) (tbh/mycmd-call import-cmd-path import-date))
       (generated-file (-second-item (s-match "\\- Wrote .* Ledger entries to file \\(.*\\).$" output))))
    (message "Importing ledger postings from %s." generated-file)
    (goto-char (point-max))
    (newline)
    (insert-file-contents generated-file)
    (ledger-mode-clean-buffer)
    (goto-char (point-min))
    (tbh/ledger-next-imported-xact)))

(defun tbh/import-capital-one-checking-csv ()
  (interactive)
  (tbh/import-csv "capital-one/checking" "capital-one-checking"))

(defun tbh/import-capital-one-money-market-csv ()
  (interactive)
  (tbh/import-csv "capital-one/money-market" "capital-one-money-market"))

(defun tbh/import-chase-visa-csv ()
  (interactive)
  (tbh/import-csv "chase-visa" "chase-visa"))

(after! ledger-mode
  (setq ledger-clear-whole-transactions nil)

  (map!
   (:localleader
    :map ledger-mode-map
    :desc "Next Imported Transaction" "n" #'tbh/ledger-next-imported-xact
    (:prefix ("i" . "Import CSVs")
     :desc "Capital One Checking" "c" #'tbh/import-capital-one-checking-csv
     :desc "Capital One Money Market" "m" #'tbh/import-capital-one-money-market-csv
     :desc "Chase Visa" "v" #'tbh/import-chase-visa-csv))))

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

(use-package! auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode t))

;; Temporarily allow ~/.authinfo until I get gpg working again
(pushnew! auth-sources "~/.authinfo")

(defun tbh/ediff-init ()
  (interactive)
  (let ((init-template-file (expand-file-name "templates/init.example.el" doom-emacs-dir)))
    (magit-ediff-compare tbh-last-init-el-template-commit "HEAD" init-template-file init-template-file)))
