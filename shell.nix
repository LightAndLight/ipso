let pkgs = (import ./. {}).pkgs; in
pkgs.mkShell {
  buildInputs = with pkgs; [
    cargo2nix
    (rust-bin.nightly.latest.default.override {
      extensions = [
        "cargo"
        "clippy"
        "rustc"
        "rust-src"
        "rustfmt"
      ];
    })
  ];
}