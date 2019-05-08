;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode . (
  (dante-repl-command-line . ("nix-shell" "--run" (concat "cabal new-repl " dante-target " --builddir=dist-newstyle/dante")))
)))
