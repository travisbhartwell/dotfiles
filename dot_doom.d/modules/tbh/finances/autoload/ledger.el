;;; tbh/finances/autoload/ledger.el -*- lexical-binding: t; -*-

;;;###autoload
(defun tbh/ledger-next-imported-xact ()
  (interactive)
  (search-forward "; SOURCE")
  (ledger-navigate-beginning-of-xact))
