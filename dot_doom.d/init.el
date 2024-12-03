;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (corfu +orderless +icons)
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
       (treemacs +lsp)
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
       (dired +ranger +icons)
       electric
       (ibuffer +icons)
       undo
       vc

       :checkers
       syntax
       spell
       grammar

       :tools
       direnv
       (eval +overlay)
       (lookup +dictionary +docsets +offline)
       (lsp +peek)
       (magit +forge)
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
       (json +lsp +tree-sitter)
       (java +lsp +tree-sitter)
       ledger
       (lua +fennel +lsp +tree-sitter)
       (markdown +grip)
       (nix +lsp +tree-sitter)
       (org +pandoc +pretty)
       (python +lsp +pyright +pyenv +poetry +tree-sitter)
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
      "a39dd36e97076459e6155158d3593dfd0ce78fc5")
