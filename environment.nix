self: super:
let
  taffybarEnvironment = ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "taffybar";
    repo = "taffybar";
    rev = "79c55146421f40092e5ac328927f72f6eed5ed43";
    sha256 = "0a0av3r2mcdn4b5z6fw433s3bzyg96ddqkbfcqv5pzfqs9d6ixd2";
  }).outPath + "/environment.nix";
  ourOverlay = _: pkgs: {
    haskellPackages = pkgs.haskellPackages.override (old: {
      overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
        unordered-containers = pkgs.haskell.lib.overrideCabal super.unordered-containers (_: {
          version = "0.2.10.0";
          sha256 = "0wy5hfrs880hh8hvp648bl07ws777n3kkmczzdszr7papnyigwb5";
        });
        binary-instances = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.overrideCabal super.binary-instances (old: {
          libraryHaskellDepends = old.libraryHaskellDepends ++ [
            self.binary-orphans_1_0_1
          ];
          broken = false;
        }));
        binary-orphans_1_0_1 = pkgs.haskell.lib.dontCheck super.binary-orphans_1_0_1;
        github = pkgs.haskell.lib.overrideCabal super.github (old: {
          version = "0.22";
          sha256 = "15py79qcpj0k331i42njgwkirwyiacbc5razmxnm4672dvvip2qk";
          libraryHaskellDepends = old.libraryHaskellDepends ++ [
            self.binary-instances self.exceptions self.transformers-compat
          ];
          broken = false;
        });
        time-compat = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.overrideCabal super.time-compat (old: {
          version = "1.9.2.2";
          sha256 = "05va0rqs759vbridbcl6hksp967j9anjvys8vx72fnfkhlrn2s52";
          libraryHaskellDepends = old.libraryHaskellDepends ++ [
            self.base-orphans
          ];
        }));
      });
    });
  };
in super.lib.composeExtensions ourOverlay (import taffybarEnvironment) self super
