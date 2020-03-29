with import ./nixpkgs.nix {};

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (p: with p; [
      ansi-terminal
      cabal-install
      pandoc
    ]))
  ];
}
