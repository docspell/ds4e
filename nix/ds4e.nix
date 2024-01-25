{ lib, emacs }:

let
  version =
    lib.fileContents ../version.txt;
in
emacs.pkgs.trivialBuild rec {
  pname = "ds4e";
  inherit version;
  src = ../lisp;
  patchPhase = ''
    sed -i 's,ds4e-version "unknown",ds4e-version "${version}",g' ds4e-settings.el
  '';
  packageRequires = with emacs.pkgs; [ dashboard ];
}
