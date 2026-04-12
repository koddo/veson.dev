let
  sources = import ./npins;
  pkgs = import sources.nixpkgs {};
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs_24
    pandoc
  ];
}
