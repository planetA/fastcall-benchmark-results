{
  description = "A devShell example";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        fontsConf = pkgs.makeFontsCache {
          fontDirectories = [
            "${pkgs.ghostscript}/share/ghostscript/fonts"
          ];
        };
      in
      with pkgs;
      {
        devShells.default = mkShell {
          buildInputs = with python311Packages ; [
            ipython
            pip
            matplotlib
            ipdb
            numpy
            libertinus
            libertine
          ];

          # FONTCONFIG_FILE = fontsConf;

          runScript = "bash";
        };
      }
    );
}
