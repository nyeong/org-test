{
  description = "org-test - Literal testing tools for org-mode";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Batch용 Emacs with org-mode and async
        testEmacs = pkgs.emacs-nox.pkgs.withPackages (epkgs: [
          epkgs.org
          epkgs.async
        ]);
        
        # Test runner script
        testScript = pkgs.writeShellScriptBin "check" ''
          ${testEmacs}/bin/emacs --batch -l org-test.el tests/api.org --eval "(org-test-run-current-buffer)"
        '';
        
        # Pre-commit hooks
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            org-test = {
              enable = true;
              name = "org-test";
              description = "Run org-test tests";
              entry = "${testScript}/bin/check";
              pass_filenames = false;
            };
          };
        };
        
      in {
        packages = {
          default = testEmacs;
          test = testScript;
        };
        
        # Checks include both tests and pre-commit hooks
        checks = {
          pre-commit = pre-commit-check;
          tests = pkgs.runCommand "org-test-check" {
            buildInputs = [ testEmacs ];
          } ''
            cp -r ${./.}/* .
            chmod -R +w .
            ${testEmacs}/bin/emacs --batch -l org-test.el tests/api.org --eval "(org-test-run-current-buffer)"
            touch $out
          '';
        };
        
        devShells.default = pkgs.mkShell {
          buildInputs = [
            testEmacs
            testScript
          ];
          
          shellHook = pre-commit-check.shellHook + ''
            echo ""
            echo "org-test development environment"
            echo ""
            echo "Commands:"
            echo "  check    - Run all tests"
            echo ""
          '';
        };
      }
    );
}

