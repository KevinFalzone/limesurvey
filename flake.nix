{
  description = "Development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          pkgs.R
          pkgs.curl
          pkgs.gcc
          pkgs.libxml2
          pkgs.openssl
          pkgs.pkg-config
          pkgs.quarto
        ];

        shellHook = ''
          export LD_LIBRARY_PATH=${pkgs.openssl.out}/lib:$LD_LIBRARY_PATH
        '';
      };
    };
}
