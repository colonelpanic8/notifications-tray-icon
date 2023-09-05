{
  description = "Notifications Tray Icon";
  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    git-ignore-nix = {
      url = "github:IvanMalison/gitignore.nix/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-language-server = {
      url = "github:colonelpanic8/haskell-language-server/goto-dependency-definition-2";
    };
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix, haskell-language-server, ... }:
  let
    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = hfinal: hprev: prev.haskell.packageOverrides hfinal hprev // {
          notifications-tray-icon =
            hfinal.callCabal2nix "notifications-tray-icon"
            (git-ignore-nix.lib.gitignoreSource ./.)
            { };
        };
      };
    };
    overlays = [overlay];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
      hpkgs = pkgs.haskell.packages.ghc945;
  in
  {
    devShell = hpkgs.shellFor {
      packages = p: [ p.notifications-tray-icon ];
      nativeBuildInputs = [
        haskell-language-server.packages.${system}.haskell-language-server-94
      ];
    };
    defaultPackage = hpkgs.notifications-tray-icon;
  }) // { inherit overlay overlays; } ;
}
