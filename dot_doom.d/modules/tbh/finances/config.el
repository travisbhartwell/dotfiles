;;; tbh/finances/config.el -*- lexical-binding: t; -*-

(use-package! dash)
(use-package! f)

(setq ledger-post-amount-alignment-column 68)

(after! ledger-mode
  (setq ledger-clear-whole-transactions nil)

  (map!
   (:localleader
    :map ledger-mode-map
    :desc "Next Imported Transaction" "n" #'tbh/ledger-next-imported-xact
    (:prefix ("i" . "Import CSVs")
     :desc "Capital One Checking" "c" #'tbh/import-capital-one-checking-csv
     :desc "Capital One Money Market" "m" #'tbh/import-capital-one-money-market-csv
     :desc "Capital One Savings" "s" #'tbh/import-capital-one-savings-csv
     :desc "Chase Visa" "v" #'tbh/import-chase-visa-csv)
    (:prefix ("o" . "Open Statements")
     :desc "Capital One" "c" #'tbh/open-capital-one-statement
     :desc "Charles Schwab Investment" "i" #'tbh/open-charles-schwab-investment-statement
     :desc "Charles Schwab Rollover IRA" "r" #'tbh/open-charles-schwab-rollover-ira-statement
     :desc "Chase Visa" "v" #'tbh/open-chase-visa-statement))))
