{
  description = "fzfx — FZF file picker with ripgrep integration and tmux output";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems f;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.haskellPackages.callCabal2nix "fzfx" (pkgs.lib.cleanSource ./.) {};
        }
      );

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.haskellPackages.shellFor {
            packages = _: [ self.packages.${system}.default ];
            buildInputs = [
              pkgs.cabal-install
              pkgs.haskellPackages.haskell-language-server
            ];
          };
        }
      );
    };
}
