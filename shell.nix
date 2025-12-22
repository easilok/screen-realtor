{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = with pkgs; [ sbcl rlwrap openssl.dev ];
  shellHook = ''
    echo CommonLisp dev shell
    export LD_LIBRARY_PATH=${pkgs.openssl.out}/lib:${pkgs.sqlite.out}/lib
  '';
}
