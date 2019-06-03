let
  overlay = (_: pkgs: {
    haskellPackages = (import ./overlay.nix) pkgs;
  });
  pkgs = import <nixpkgs> { overlays = [ overlay ]; };
in pkgs.haskellPackages.callPackage ./notifications-tray-icon.nix { }
