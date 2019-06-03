{ mkDerivation, aeson, async, base, bytestring, containers, dbus
, gi-dbusmenu, gi-gio, gi-glib, github, haskeline, hpack, hslogger
, http-conduit, http-types, optparse-applicative, process
, regex-compat, status-notifier-item, stdenv, text, transformers
, tuple, vector
}:
mkDerivation {
  pname = "notifications-tray-icon";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring containers dbus gi-dbusmenu gi-gio
    gi-glib github hslogger http-conduit http-types process
    regex-compat status-notifier-item text transformers vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring github haskeline hslogger optparse-applicative text
    transformers tuple
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/IvanMalison/notifications-tray-icon#readme";
  license = stdenv.lib.licenses.bsd3;
}
