FROM debian:trixie AS build-server

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

COPY CHANGELOG.md LICENSE.md README.md /water-wars/
COPY client/ /water-wars/client
COPY library/ /water-wars/library
COPY server/ /water-wars/server
COPY test-suite/ /water-wars/test-suite
COPY resources/ /water-wars/resources
COPY static/ /water-wars/static
COPY cabal.project cabal.wasm.project Makefile water-wars.cabal cabal.docker.project /water-wars/

WORKDIR /water-wars

RUN cabal install --project-file cabal.docker.project -j --semaphore exe:water-wars-server --installdir /water-wars/bin --install-method=copy

FROM debian:trixie

COPY --from=build-server /water-wars/bin/ /water-wars/bin
COPY public/ /water-wars/public
COPY resources/ /water-wars/resources
