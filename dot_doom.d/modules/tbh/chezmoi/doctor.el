;;; tbh/chezmoi/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "chezmoi")
  (warn! "Couldn't find the chezmoi binary."))
