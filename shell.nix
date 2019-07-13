with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "haskell-ide-engine";
  buildInputs = [
    gmp
    zlib
    ncurses
 
    mesa_noglu
    freeglut
    libpulseaudio
 ];
  src = null;
  shellHook = ''

    export LD_LIBRARY_PATH=${mesa_noglu.drivers}/lib:${freeglut}/lib:${libpulseaudio}/lib:${gmp}/lib:${zlib}/lib:${ncurses}/lib
    export PATH=$PATH:$HOME/.local/bin
  '';
}
