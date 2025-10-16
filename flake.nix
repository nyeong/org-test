{
  description = "org-test - Literate testing tools for org-mode";

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
        
        # Use emacs-nox to avoid graphical dependencies
        testEmacs = pkgs.emacs-nox.pkgs.withPackages (epkgs: [
          epkgs.org
        ]);
        
        # Test runner script
        testScript = pkgs.writeShellScriptBin "check" ''
          ${testEmacs}/bin/emacs --batch \
            --eval '(setq org-confirm-babel-evaluate nil)' \
            --eval '(org-babel-do-load-languages '"'"'org-babel-load-languages '"'"'((emacs-lisp . t) (C . t) (shell . t)))' \
            -l org-test.el \
            --eval '(org-test-run "examples/")'
        '';
        
        # Call graph generator script
        callGraphScript = pkgs.writeShellScriptBin "generate-call-graph" ''
          echo "Generating call graph..."
          SOURCE_FILE=org-test.el DOT_FILE=call-graph.dot \
            ${testEmacs}/bin/emacs --batch -l scripts/call-graph.el
          ${pkgs.graphviz}/bin/dot -Tsvg call-graph.dot -o examples/call_graph.svg
          echo "Call graph generated: examples/call_graph.svg"
        '';
        
        # Pre-commit hooks
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            call-graph = {
              enable = true;
              name = "call-graph";
              description = "Generate call graph";
              entry = "${callGraphScript}/bin/generate-call-graph";
              pass_filenames = false;
              stages = ["pre-commit"];
            };
            org-test = {
              enable = true;
              name = "org-test";
              description = "Run org-test tests";
              entry = "${testScript}/bin/check";
              pass_filenames = false;
              stages = ["pre-commit"];
            };
          };
        };
        
      in {
        packages = {
          default = testEmacs;
          test = testScript;
        };
        
        # Checks include both examples and pre-commit hooks
        checks = {
          pre-commit = pre-commit-check;
          examples = pkgs.runCommand "org-test-check" {
            buildInputs = [ testEmacs ];
          } ''
            cp -r ${./.}/* .
            chmod -R +w .
            ${testEmacs}/bin/emacs --batch \
              --eval '(setq org-confirm-babel-evaluate nil)' \
              --eval '(org-babel-do-load-languages '"'"'org-babel-load-languages '"'"'((emacs-lisp . t) (C . t) (shell . t)))' \
              -l org-test.el \
              --eval '(org-test-run "examples/")'
            touch $out
          '';
        };
        
        devShells.default = pkgs.mkShell {
          buildInputs = [
            testEmacs
            testScript
            callGraphScript
            pkgs.graphviz
          ];
          
          shellHook = pre-commit-check.shellHook + ''
            echo ""
            echo "org-test development environment"
            echo ""
            echo "Commands:"
            echo "  check               - Run all tests"
            echo "  generate-call-graph - Generate call graph SVG"
            echo ""
          '';
        };
      }
    );
}

