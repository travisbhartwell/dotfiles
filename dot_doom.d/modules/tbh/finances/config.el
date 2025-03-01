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
    (:prefix ("c" . "Cleared Balance")
             (:prefix ("c" . "Capital One")
              :desc "Checking" "c" #'tbh/check-capital-one-checking-cleared
              :desc "Savings" "s" #'tbh/check-capital-one-savings-cleared)
             (:prefix ("i" . "Charles Schwab Investments")
              :desc "Bank" "b" #'tbh/check-schwab-bank-cleared
              :desc "Investment" "i" #'tbh/check-schwab-investment-cleared)
             (:prefix ("r" . "Charles Schwab Rollover IRA")
              :desc "Bank" "b" #'tbh/check-schwab-rollover-ira-bank-cleared
              :desc "Investment" "i" #'tbh/check-schwab-rollover-ira-investment-cleared)
             :desc "Chase Visa" "v" #'tbh/check-visa-cleared)
    (:prefix ("i" . "Import CSVs")
     :desc "Capital One Checking" "c" #'tbh/import-capital-one-checking-csv
     :desc "Capital One Savings" "s" #'tbh/import-capital-one-savings-csv
     :desc "Chase Visa" "v" #'tbh/import-chase-visa-csv)
    (:prefix ("o" . "Open Statements")
     :desc "Capital One" "c" #'tbh/open-capital-one-statement
     :desc "Charles Schwab Investment" "i" #'tbh/open-charles-schwab-investment-statement
     :desc "Charles Schwab Rollover IRA" "r" #'tbh/open-charles-schwab-rollover-ira-statement
     :desc "Chase Visa" "v" #'tbh/open-chase-visa-statement))))

(after! git-commit
  (map!
   (:localleader
    :map git-commit-mode-map
    (:prefix ("f" . "Account Finished")
     :desc "Capital One Checking" "c" #'tbh/insert-checking-finished-line
     :desc "Capital One Savings" "s" #'tbh/insert-savings-finished-line
     :desc "Charles Schwab Investment" "i" #'tbh/insert-investment-finished-line
     :desc "Charles Schwab Rollover IRA" "r" #'tbh/insert-rollover-ira-finished-line
     :desc "Chase Visa" "v" #'tbh/insert-visa-finished-line))))
