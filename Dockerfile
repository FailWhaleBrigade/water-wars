FROM debian:forky

RUN apt update && apt upgrade -y && apt install -y \
    curl \
    build-essential \
    libffi-dev libffi8 \
    libgmp-dev libgmp10 \
    libncurses-dev libncurses6 \
    libtinfo6 \
    pkg-config \
    zlib1g-dev \
    patch \
    git

RUN BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH=/root/.ghcup/bin:$PATH


RUN ghcup install --set ghc 9.14.1

COPY client/ /water-wars/client
COPY library/ /water-wars/library
COPY client/ /water-wars/server
COPY resources/ water-wars/resources
COPY static/ water-wars/static
COPY cabal.project cabal.wasm.project Makefile water-wars.cabal /water-wars/

WORKDIR /water-wars

RUN cabal install -j --semaphore exe:water-wars-server --installdir /water-wars/bin --install-method=copy

COPY public/ /water-wars/public
