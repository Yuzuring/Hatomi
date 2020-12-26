let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs: with pkgs; [
    aeson
    http-client
    http-client-tls
    terminal-size
    clock
    async
    ilist
  ];
  ghc = pkgs.ghc.withPackages haskellPackages;
in
pkgs.stdenv.mkDerivation {
  name = "Hatomi";
  src = ./src;
  buildInputs = [ ghc ];
  buildPhase = ''
    ghc Main.hs -O3 -o hatomi
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp hatomi $out/bin/hatomi
  '';
}
