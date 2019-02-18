{ mkDerivation, base, random, stdenv }:
mkDerivation {
  pname = "roulette";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base random ];
  homepage = "https://github.com/willbush/roulette#readme";
  description = "A simplified text based roulette game";
  license = stdenv.lib.licenses.mit;
}
