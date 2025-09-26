;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (corfu +icons +orderless)
       (vertico +icons)

       :ui
       doom
       doom-dashboard
       (emoji +ascii +github +unicode)
       hl-todo
       indent-guides
       ligatures
       modeline
       nav-flash
       neotree
       ophints
       (popup +all +defaults)
       tabs
       treemacs
       unicode
       (vc-gutter +pretty)
       vi-tilde-fringe
       (window-select +numbers)
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       snippets
       word-wrap

       :emacs
       (dired +dirvish +icons)
       electric
       (ibuffer +icons)
       tramp
       undo
       vc

       :checkers
       (syntax +icons)
       (spell +everywhere)
       grammar

       :tools
       direnv
       (docker +lsp +tree-sitter)
       (eval +overlay)
       (lookup +dictionary +docsets +offline)
       (lsp +booster +eglot)
       magit
       make
       tmux
       tree-sitter

       :os
       (:if (featurep :system 'macos) macos)

       :lang
       (cc +lsp +tree-sitter)
       (clojure +lsp +tree-sitter)
       data
       emacs-lisp
       (go +lsp +tree-sitter)
       graphviz
       (haskell +lsp +tree-sitter)
       (janet +tree-sitter)
       (java +lsp +tree-sitter)
       (json +lsp +tree-sitter)
       ledger
       (lua +fennel +lsp +tree-sitter)
       (markdown +grip)
       (nix +lsp +tree-sitter)
       (org +journal +pandoc +roam)
       (python +lsp +pyright +pyenv +poetry +tree-sitter)
       (ruby +lsp +rails +tree-sitter)
       (rust +lsp +tree-sitter)
       (sh +lsp +tree-sitter)
       (swift +lsp +tree-sitter)
       (web +lsp +tree-sitter)
       (yaml +lsp +tree-sitter)
       (zig +lsp +tree-sitter)

       :tbh
       chezmoi
       finances
       mycmd
       notes

       :config
       (default +bindings +smartparens))

;; This is the commit of the Doom Emacs source that this is based upon
;; TODO: Add function to update this automatically
(setq tbh-last-init-el-template-commit
      "63653091642a60d378a4dcf96bcf811c45d35cc7")
