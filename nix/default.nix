{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, customConfig ? {}
, sourcesOverride ? {}
, gitrev ? null
}:
let
  flakeSources = let
    flakeLock = (builtins.fromJSON (builtins.readFile ../flake.lock)).nodes;
    compat = s: builtins.fetchGit {
      url = "https://github.com/${s.locked.owner}/${s.locked.repo}.git";
      inherit (s.locked) rev;
      ref = s.original.ref or "master";
    };
  in {
    "haskell.nix" = compat flakeLock.haskellNix;
    "tbco-nix" = compat flakeLock.tbcoNix;
  };
  sources = flakeSources // sourcesOverride;
  haskellNix = import sources."haskell.nix" { inherit system sourcesOverride; };
  # IMPORTANT: report any change to nixpkgs channel in flake.nix:
  nixpkgs = haskellNix.sources.nixpkgs-2105;
  tbcoNix = import sources.tbco-nix { inherit system; };
  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/The-Blockchain-Company/haskell.nix)
    haskellNix.nixpkgsArgs.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ tbcoNix.overlays.haskell-nix-extra
    ++ tbcoNix.overlays.crypto
    # tbcoNix: nix utilities:
    ++ tbcoNix.overlays.tbcoNix
    ++ tbcoNix.overlays.utils
    # our own overlays:
    ++ [
      (pkgs: _: {
        gitrev = if gitrev == null
          then tbcoNix.commitIdFromGitRepoOrZero ../.git
          else gitrev;
        customConfig = pkgs.lib.recursiveUpdate
          (import ./custom-config.nix pkgs.customConfig)
          customConfig;
        inherit (pkgs.tbcoNix) bccLib;
        # commonLib: mix pkgs.lib with tbco-nix utils and our own:
        commonLib = with pkgs; lib // bccLib // tbco-nix.lib
          // import ./util.nix { inherit haskell-nix; }
          // import ./svclib.nix { inherit pkgs; }
          # also expose our sources, nixpkgs and overlays
          // { inherit overlays sources nixpkgs; };
      })
      # And, of course, our haskell-nix-ified cabal project:
      (import ./pkgs.nix)
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.nixpkgsArgs.config // config;
  };

in pkgs
