{
  description = "Notifications Tray Icon";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix.url = "github:IvanMalison/gitignore.nix/master";
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix }:
  let
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
          notifications-tray-icon =
            hself.callCabal2nix "notifications-tray-icon"
            (git-ignore-nix.gitIgnoreSource ./.)
            { };
        });
      });
    };
    overlays = [overlay];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.notifications-tray-icon ];
    };
    defaultPackage = pkgs.haskellPackages.notifications-tray-icon;
  }) // { inherit overlay overlays; } ;
}
