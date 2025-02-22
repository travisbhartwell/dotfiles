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

;;;###autoload
(defun tbh/mycmd-project-tasks ()
  (-let (((_status . output) (tbh/mycmd-call "project" "list-tasks" "--quiet")))
    (s-lines output)))

;;;###autoload
(defun tbh/mycmd-project-run-quiet (&rest args)
  (let ((project-run-args (append '("project" "run" "--quiet" "--") args)))
    (apply #'tbh/mycmd-call project-run-args)))

;;;###autoload
(defun tbh/mycmd-project-run-compilation (task)
  (let ((cmd-line (s-join " " `("mycmd" "project" "run" ,task))))
    (compile cmd-line)))

;;;###autoload
(defun tbh/mycmd-project-run-task-compilation ()
  (interactive)
  (let ((task (completing-read "Select Project Task to Run: " (tbh/mycmd-project-tasks) nil t nil nil)))
    (tbh/mycmd-project-run-compilation task)))
