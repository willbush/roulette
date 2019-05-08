let
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-19-03.json; };
  myPackages = (import ./release.nix { withHoogle = true; } );

  projectDrvEnv = myPackages.project1.env.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [ pinnedPkgs.haskellPackages.cabal-install ];
  });
in
  projectDrvEnv
