;;; tbh/mycmd/config.el -*- lexical-binding: t; -*-

(defvar tbh/mycmd-executable
  (executable-find "mycmd")
  "The full path of the MyCmd launcher, typically bin/mycmd in the MyCmd
install location.")

(use-package! s)

