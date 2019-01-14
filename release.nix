with builtins;
with import ./nixpkgs.nix;
with lib;
with {
  nixpkgsVersion = fileContents "${path}/.version";
  ghcVersion     = haskellPackages.ghc.version;
};
{
  "nixpkgs${nixpkgsVersion}-ghc${ghcVersion}-runtime-arbitrary-tests" =
    haskellPackages.runtime-arbitrary-tests;
}
