;;; tbh/finances/autoload/commit.el -*- lexical-binding: t; -*-

;;;###autoload
(defun tbh/insert-account-finished-line (account)
  "Insert 'FINISHED: ' followed by ACCOUNT on a new line, placing the cursor after the colon and space."
  (interactive)
  (insert "\nFINISHED: " account)
  (goto-char (- (point) (length account))))

;;;###autoload
(defun tbh/insert-checking-finished-line ()
  (interactive)
  (tbh/insert-account-finished-line "Capital One 360 Checking"))

;;;###autoload
(defun tbh/insert-money-market-finished-line ()
  (interactive)
  (tbh/insert-account-finished-line "Capital One 360 Money Market"))

;;;###autoload
(defun tbh/insert-savings-finished-line ()
  (interactive)
  (tbh/insert-account-finished-line "Capital One 360 Performance Savings"))

;;;###autoload
(defun tbh/insert-investment-finished-line ()
  (interactive)
  (tbh/insert-account-finished-line "Charles Schwab Investment"))

;;;###autoload
(defun tbh/insert-rollover-ira-finished-line ()
  (interactive)
  (tbh/insert-account-finished-line "Charles Schwab Rollover IRA"))

;;;###autoload
(defun tbh/insert-visa-finished-line ()
  (interactive)
  (tbh/insert-account-finished-line "Amazon Rewards Visa"))
