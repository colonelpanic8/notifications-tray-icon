self: super:

let
  sourceTransformer =
    if builtins.getEnv "CI" == "" && builtins.pathExists ./.git
    then builtins.fetchGit
    else (x: x);
  thisOverlay = _: pkgs: {
    haskellPackages = pkgs.haskellPackages.override (old: {
      overrides =
        pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
        (self: super: {
          notifications-tray-icon =
            self.callCabal2nix "notifications-tray-icon" (sourceTransformer ./.)
            { };
        });
    });
  };
in super.lib.composeExtensions thisOverlay (import ./environment.nix) self super
