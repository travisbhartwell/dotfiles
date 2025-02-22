;;; tbh/finances/autoload/import.el -*- lexical-binding: t; -*-

;;;###autoload
(defun tbh/get-source-csvs (account)
  "Get the available source CSVs for ACCOUNT."
  (-let*
      (((_status . output) (tbh/mycmd-call "financial" "list" "source-csvs"))
       (output-lines (-filter (lambda (line) (s-starts-with? account line)) (s-lines output))))
    (-map (lambda (line) (-drop 1 (s-split " " line))) output-lines)))

;;;###autoload
(defun tbh/get-most-recent (account)
  "Get the most recent reconciled month for ACCOUNT."
  (-let*
      (((_status . output) (tbh/mycmd-project-run-quiet (s-concat "most-recent-" account)))
       (output-lines (-filter (lambda (line) (s-starts-with? "    " line)) (s-lines output))))
    (s-trim (car output-lines))))

;;;###autoload
(defun tbh/get-next-month (month)
  "Get the next month given MONTH."
  (-let
      (((_status . output) (tbh/mycmd-call "financial" "next-month" month)))
    (s-trim output)))

;;;###autoload
(defun tbh/next-month-for-account (account)
  "Get the next month to be imported for ACCOUNT."
  (-let
      ((most-recent (tbh/get-most-recent account)))
    (tbh/get-next-month most-recent)))

;;;###autoload
(defun tbh/import-csv (arg account import-command)
  (interactive "P")
  (-let*
      ((import-date
        (if arg
            (completing-read "Select date to import: " (tbh/get-source-csvs account) nil t nil nil)
          (tbh/next-month-for-account import-command)))
       ((_status . output) (tbh/mycmd-call "financial" "import" import-command import-date))
       (generated-file (-second-item (s-match "\\- Wrote .* Ledger entries to file \\(.*\\).$" output))))
    (message "Importing ledger postings from %s." generated-file)
    (goto-char (point-max))
    (newline)
    (insert-file-contents generated-file)
    (ledger-mode-clean-buffer)
    (goto-char (point-min))
    (tbh/ledger-next-imported-xact)))

;;;###autoload
(defun tbh/import-capital-one-checking-csv (arg)
  (interactive "P")
  (tbh/import-csv arg "capital-one/checking" "capital-one-checking"))

;;;###autoload
(defun tbh/import-capital-one-money-market-csv (arg)
  (interactive "P")
  (tbh/import-csv arg "capital-one/money-market" "capital-one-money-market"))

;;;###autoload
(defun tbh/import-capital-one-savings-csv (arg)
  (interactive "P")
  (tbh/import-csv arg "capital-one/savings" "capital-one-savings"))

;;;###autoload
(defun tbh/import-chase-visa-csv (arg)
  (interactive "P")
  (tbh/import-csv arg "chase-visa" "chase-visa"))
