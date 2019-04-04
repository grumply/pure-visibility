{ mkDerivation, base, pure, pure-cond, pure-prop, stdenv }:
mkDerivation {
  pname = "pure-visibility";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-cond pure-prop ];
  homepage = "github.com/grumply/pure-visibility";
  description = "Element Visibility wrapper component";
  license = stdenv.lib.licenses.bsd3;
}
