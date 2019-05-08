{ mkDerivation, base, random, rio, stdenv, text }:
mkDerivation {
  pname = "roulette";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base random rio text ];
  homepage = "https://github.com/willbush/roulette#readme";
  description = "A simplified text based roulette game";
  license = stdenv.lib.licenses.mit;
}
