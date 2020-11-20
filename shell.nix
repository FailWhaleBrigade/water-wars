with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "water-wars";
  buildInputs = [
    gmp
    zlib
    ncurses
 
    mesa_noglu
    libGLU
    libGL

    cabal-install
    freeglut
    libpulseaudio
 ];
  src = null;
  shellHook = ''

    export LD_LIBRARY_PATH=${libGL}/lib:${libGLU}/lib:${mesa_noglu.drivers}/lib:${freeglut}/lib:${libpulseaudio}/lib:${gmp}/lib:${zlib}/lib:${ncurses}/lib
    export PATH=$PATH:$HOME/.local/bin
  '';
}
