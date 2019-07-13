with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "haskell-ide-engine";
  buildInputs = [
    gmp
    zlib
    ncurses
 
    mesa_noglu
    libGLU_combined
    freeglut
    libpulseaudio
 ];
  src = null;
  shellHook = ''

    export LD_LIBRARY_PATH=${libGLU_combined}/lib:${mesa_noglu.drivers}/lib:${freeglut}/lib:${libpulseaudio}/lib:${gmp}/lib:${zlib}/lib:${ncurses}/lib
    export PATH=$PATH:$HOME/.local/bin
  '';
}
