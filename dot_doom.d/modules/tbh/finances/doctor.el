;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; tbh/finances/doctor.el

(assert! (modulep! :lang ledger)
         "This module requires (:lang ledger)")

(assert! (modulep! :tbh mycmd)
         "This module requires (:tbh mycmd)")
