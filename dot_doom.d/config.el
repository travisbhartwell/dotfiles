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
(let ((tbh-font-family
       (seq-find #'doom-font-exists-p ["Monaspace Krypton" "SauceCodePro NF"])))
  (setq doom-font (font-spec :family tbh-font-family :size 12)))

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

(defun tbh/ediff-init ()
  (interactive)
  (ediff (f-expand "~/.emacs.d/templates/init.example.el") (f-expand "~/.local/share/chezmoi/dot_doom.d/init.el")))

;; we recommend using use-package to organize your init.el
(use-package codeium
  ;; if you use straight
  ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
  ;; otherwise, make sure that the codeium.el file is on load-path

  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  ;; or on a hook
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

  ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local completion-at-point-functions
  ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
  ;; an async company-backend is coming soon!

  ;; codeium-completion-at-point is autoloaded, but you can
  ;; optionally set a timer, which might speed up things as the
  ;; codeium local language server takes ~0.2s to start up
  ;; (add-hook 'emacs-startup-hook
  ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

  ;; :defer t ;; lazy loading, if you want
  :config
  (setq use-dialog-box nil) ;; do not use popup boxes

  ;; if you don't want to use customize to save the api-key
  ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; alternatively for a more extensive mode-line
  ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; you can also set a config for a single buffer like this:
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local codeium/editor_options/tab_size 4)))

  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;; Allows for editing files for use with https://github.com/booniepepper/dsg-md-posix/
(add-to-list 'auto-mode-alist '("\\.md\\.part\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.template\\'" . html-mode))
