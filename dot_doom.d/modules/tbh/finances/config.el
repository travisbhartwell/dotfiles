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
     :desc "Chase Visa" "v" #'tbh/import-chase-visa-csv))))
