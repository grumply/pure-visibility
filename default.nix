{ mkDerivation, base, pure, pure-cond, stdenv }:
mkDerivation {
  pname = "pure-visibility";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-cond ];
  homepage = "github.com/grumply/pure-visibility";
  description = "Styled views";
  license = stdenv.lib.licenses.bsd3;
}
