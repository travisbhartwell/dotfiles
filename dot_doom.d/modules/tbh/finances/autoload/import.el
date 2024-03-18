;;; tbh/finances/autoload/import.el -*- lexical-binding: t; -*-

;;;###autoload
(defun tbh/get-source-csvs (account)
  "Get the available source CSVs for ACCOUNT."
  (-let*
      (((_status . output) (tbh/mycmd-call "financial" "list-source-csvs"))
       (output-lines (-filter (lambda (line) (s-starts-with? account line)) (s-lines output))))
    (-map (lambda (line) (-drop 1 (s-split " " line))) output-lines)))

;;;###autoload
(defun tbh/import-csv (account import-command)
  (interactive)
  (-let*
      ((import-date (completing-read "Select date to import: " (tbh/get-source-csvs account) nil t nil nil))
       ((_status . output) (tbh/mycmd-call "financial" "import" import-command import-date))
       (generated-file (-second-item (s-match "\\- Wrote .* Ledger entries to file \\(.*\\).$" output))))
    (message "Importing ledger postings from %s." generated-file)
    (goto-char (point-max))
    (newline)
    (insert-file-contents generated-file)
    (+format-buffer-h)
    (goto-char (point-min))
    (tbh/ledger-next-imported-xact)))

;;;###autoload
(defun tbh/import-capital-one-checking-csv ()
  (interactive)
  (tbh/import-csv "capital-one/checking" "capital-one-checking"))

;;;###autoload
(defun tbh/import-capital-one-money-market-csv ()
  (interactive)
  (tbh/import-csv "capital-one/money-market" "capital-one-money-market"))

;;;###autoload
(defun tbh/import-chase-visa-csv ()
  (interactive)
  (tbh/import-csv "chase-visa" "chase-visa"))

