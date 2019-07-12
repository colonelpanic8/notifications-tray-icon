let
  pkgs = import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; };
in pkgs.haskellPackages.notifications-tray-icon
