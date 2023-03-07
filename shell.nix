with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    emacs clojure html-tidy
  ];
  shellHook = ''
  '';
}
