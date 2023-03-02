{
  # nixConfig = {
  #   bash-prompt-suffix = "[dev]";
  # };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flakeUtils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    easyPSSrc = {
      flake = false;
      url = "github:justinwoo/easy-purescript-nix";
    };
  };

  outputs = { self, nixpkgs, flakeUtils, easyPSSrc }:
    flakeUtils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        easyPS = pkgs.callPackage easyPSSrc { inherit pkgs; };
        nodejs-16 = pkgs.writeShellScriptBin "nodejs-16" ''
          ${ pkgs.nodejs-16_x.out}/bin/node $@
        '';
      in {
        devShell = pkgs.mkShell {
          buildInputs = [

            # Please update spago and purescript in `package.json` `scripts` section
            easyPS."purs-0_15_7"
            easyPS.purescript-language-server
            easyPS.pscid
            easyPS.purs-tidy
            easyPS.pulp
            easyPS.spago

            pkgs.jq
            pkgs.docker
            pkgs.nodePackages.bower
            pkgs.nodePackages.jshint
            pkgs.nodePackages.nodemon
            pkgs.nodePackages.yarn
            pkgs.nodePackages.webpack
            pkgs.nodePackages.webpack-cli
            pkgs.nodePackages.webpack-dev-server
            pkgs.dhall
            pkgs.nodejs-18_x
            nodejs-16
            pkgs.pkgconfig
            pkgs.postgresql
            pkgs.python27
            pkgs.python37
            pkgs.unzip
            pkgs.nixpacks
        ];
        shellHook = ''
          npm install
          NODE_OPTIONS=--experimental-fetch --trace-warnings
          export PATH=$PATH:./node_modules/.bin/:./bin
          export PS1="\n\[\033[1;32m\][nix develop:\w]\$\[\033[0m\] ";
          '';
      };
    }
  );
}
