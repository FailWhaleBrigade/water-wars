{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    ghc-wasm-meta = {
      url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
      flake = true;
    };
  };

  outputs = { self, nixpkgs, ghc-wasm-meta }:
    let
      system = "x86_64-linux"; # adjust if needed: aarch64-linux, x86_64-darwin, aarch64-darwin
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          # GHC WASM toolchain (includes wasm32-wasi-ghc, wasm32-wasi-cabal, etc.)
          ghc-wasm-meta.packages.${system}.all_9_10
          
          # Rust toolchain
          pkgs.rustc
          pkgs.cargo
          
          # HTTP server
          pkgs.simple-http-server
        ];

        shellHook = ''
          echo "GHC WASM + Rust environment loaded"
          echo "Tools available: wasm32-wasi-ghc, wasm32-wasi-cabal, rustc, cargo, simple-http-server"
          echo "Run: make && make serve"
        '';
      };
    };
}
