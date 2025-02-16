;;; tbh/finances/autoload/open.el -*- lexical-binding: t; -*-

;;;###autoload
(defun tbh/get-statements (account)
  "Get the available statements for ACCOUNT."
  (-let*
      (((_status . output) (tbh/mycmd-call "financial" "list" "statements"))
       (output-lines (-filter (lambda (line) (s-starts-with? account line)) (s-lines output))))
    (-map (lambda (line) (-drop 1 (s-split " " line))) output-lines)))

;;;###autoload
(defun tbh/open-statement (account)
  (interactive)
  (let
      ((statement-date (completing-read "Select date to open: " (tbh/get-statements account) nil t nil nil)))
    (tbh/mycmd-call "financial" "open" "statement" account statement-date)))

;;;###autoload
(defun tbh/open-capital-one-statement ()
  (interactive)
  (tbh/open-statement "capital-one"))

;;;###autoload
(defun tbh/open-charles-schwab-investment-statement ()
  (interactive)
  (tbh/open-statement "charles-schwab/investment-account"))

;;;###autoload
(defun tbh/open-charles-schwab-rollover-ira-statement ()
  (interactive)
  (tbh/open-statement "charles-schwab/rollover-ira"))

;;;###autoload
(defun tbh/open-chase-visa-statement ()
  (interactive)
  (tbh/open-statement "chase-visa"))
