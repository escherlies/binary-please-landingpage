{
  description = "Development environment for page-binary-please";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Elm tooling
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-test
            elmPackages.elm-review

            # JavaScript/TypeScript tooling
            bun

            # Development utilities
            git
          ];

          shellHook = ''
            echo "🚀 Development environment loaded!"
            echo ""
            echo "Available tools:"
            echo "  - elm $(elm --version)"
            echo "  - bun $(bun --version)"
            echo ""
            echo "Quick start:"
            echo "  - Install dependencies: bun install"
            echo "  - Run development server: bun run dev (if configured in package.json)"
            echo ""
          '';
        };
      }
    );
}
