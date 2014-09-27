{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall
    yaml wlPprintExtras lens free text aeson unorderedContainers
    stringConversions optparseApplicative hspec safe;

in cabal.mkDerivation (self: {
  pname = "Garnett";
  version = "0.0.1.0";
  src = ./src;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    yaml wlPprintExtras lens free text aeson unorderedContainers
    stringConversions optparseApplicative hspec safe
  ];
  buildTools = [ cabalInstall ];
  enableSplitObjs = false;
})
