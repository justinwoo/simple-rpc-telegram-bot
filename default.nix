let
  pkgs = import <nixpkgs> {};

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "50f85a34f665d1d328589a9a06c92282f1c930ea";
    sha256 = "1aidjwh030wfswb227zi751bf0zbpv88mz9d035c59z26akhvidg";
  });

in pkgs.stdenv.mkDerivation {
  name = "easy-purescript";

  buildInputs = easy-ps.buildInputs ++ [
    easy-ps.inputs.purs
    easy-ps.inputs.purp
    easy-ps.inputs.psc-package-simple
    easy-ps.inputs.psc-package2nix
  ];
}
