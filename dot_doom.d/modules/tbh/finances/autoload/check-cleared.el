;;; tbh/finances/autoload/check-cleared.el -*- lexical-binding: t; -*-

;;;###autoload
(defun tbh/check-capital-one-checking-cleared ()
  (interactive)
  (tbh/mycmd-project-run-compilation "check-checking-cleared"))

;;;###autoload
(defun tbh/check-capital-one-savings-cleared ()
  (interactive)
  (tbh/mycmd-project-run-compilation "check-savings-cleared"))

;;;###autoload
(defun tbh/check-schwab-bank-cleared ()
  (interactive)
  (tbh/mycmd-project-run-compilation "check-schwab-bank-cleared"))

;;;###autoload
(defun tbh/check-schwab-investment-cleared ()
  (interactive)
  (tbh/mycmd-project-run-compilation "check-schwab-investment-cleared"))

;;;###autoload
(defun tbh/check-schwab-rollover-ira-bank-cleared ()
  (interactive)
  (tbh/mycmd-project-run-compilation "check-schwab-rollover-ira-bank-cleared"))

;;;###autoload
(defun tbh/check-schwab-rollover-ira-investment-cleared ()
  (interactive)
  (tbh/mycmd-project-run-compilation "check-schwab-rollover-ira-investment-cleared"))

;;;###autoload
(defun tbh/check-visa-cleared ()
  (interactive)
  (tbh/mycmd-project-run-compilation "check-visa-cleared"))
