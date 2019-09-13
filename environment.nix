self: super:
let
  ourOverlay = _: pkgs: {
    haskellPackages = pkgs.haskellPackages.override (old: {
      overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      });
    });
  };
in ourOverlay self super
