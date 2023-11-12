{ mkDerivation, base, containers, dlist, lib, mtl, pandoc-types
, text
}:
mkDerivation {
  pname = "pandoc-builder-monadic";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers dlist mtl pandoc-types text
  ];
  homepage = "https://github.com/414owen/pandoc-builder-monadic";
  license = lib.licenses.bsd3;
}
