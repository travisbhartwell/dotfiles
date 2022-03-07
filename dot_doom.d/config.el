;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Travis B. Hartwell"
      user-mail-address "nafai@travishartwell.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "Source Code Pro" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-vivendi)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

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

(use-package! nasm-mode
  :defer t
  :mode "\\.asm\\'")

(setq ispell-dictionary "en")

(setq +format-on-save-enabled-modes
      '(not sh-mode))

;; Borrowed from https://github.com/bkchr/nixos-config/blob/3d4634b093ba40b3a98b450baef42dca885c68ee/home/files/.doom.d/config.el#L107
(use-package! vertico-posframe
  :init
  (setq vertico-posframe-border-width 10)
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  ;; NOTE: this is needed to make sure marginalia columns don't get misaligned
  (setq marginalia-margin-threshold 250)
  :config
  (vertico-posframe-mode 1))

(defconst tbh/mycmd-executable
  (executable-find "mycmd"))

(defun tbh/mycmd-call (&rest args)
  (apply #'doom-call-process (cons tbh/mycmd-executable (remq nil args))))

(defun tbh/get-source-csvs (account)
  (-let*
      (((status . output) (tbh/mycmd-call "financial" "list-source-csvs"))
       (output-lines (-filter (lambda (line) (s-starts-with? account line)) (s-lines output))))
    (-map (lambda (line) (-drop 1 (s-split " " line))) output-lines)))

(defun tbh/ledger-next-imported-xact ()
  (interactive)
  (search-forward "; SOURCE")
  (ledger-navigate-beginning-of-xact))

(defun tbh/import-csv (account import-command)
  (interactive)
  (-let*
      ((import-date (completing-read "Select date to import: " (tbh/get-source-csvs account) nil t nil nil))
       ((status . output) (tbh/mycmd-call "financial" "import" import-command import-date))
       (generated-file (-second-item (s-match "\\- Wrote .* Ledger entries to file \\(.*\\).$" output))))
    (message "Importing ledger postings from %s." generated-file)
    (goto-char (point-max))
    (newline)
    (insert-file-contents generated-file)
    (+format-buffer-h)
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
