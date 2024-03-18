;;; tbh/mycmd/autoload/mycmd.el -*- lexical-binding: t; -*-

;;;###autoload
(defun tbh/mycmd-call (&rest args)
  "Call out to the MyCmd executable."
  (apply #'doom-call-process (cons tbh/mycmd-executable (remq nil args))))

;;;###autoload
(defun tbh/mycmd-direct-path (&rest parts)
  "Get the full path of a MyCmd command from the user directory."
  (f-expand (apply #'f-join "~/mycmd" parts)))

;;;###autoload
(defun tbh/mycmd-direct-call (cmd &rest args)
  "Call out to script that uses MyCmd as the shebang."
  (apply #'doom-call-process (cons cmd (remq nil args))))
