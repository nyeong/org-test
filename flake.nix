{
  description = "org-test - Literal testing tools for org-mode";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Batch용 Emacs with org-mode and async
        testEmacs = pkgs.emacs-nox.pkgs.withPackages (epkgs: [
          epkgs.org
          epkgs.async
        ]);
        
      in {
        packages.default = testEmacs;
        
        devShells.default = pkgs.mkShell {
          buildInputs = [
            testEmacs
          ];
          
          shellHook = ''
            echo "org-test development environment"
            echo "Run tests: emacs --batch -l org-test.el -f org-test-run"
          '';
        };
      }
    );
}

