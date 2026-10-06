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
RUN ghcup install ghc 9.10.3

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

### Install eventlog-live

# Fetch the download URL for the latest Linux x86_64 asset from GitHub API
# https://protobuf.dev/installation/
RUN apt install -y jq protobuf-compiler
# Ensure compiler version is 33.0+
RUN protoc --version


RUN cabal install -z -w ghc-9.10.3 -j --semaphore eventlog-live \
        --constraint='blockio +serialblockio'                   \
        --constraint='grpc-spec -snappy'                        \
        --installdir /water-wars/bin --install-method=copy

FROM debian:trixie

COPY --from=build-server /water-wars/bin/ /water-wars/bin
COPY public/ /water-wars/public
COPY resources/ /water-wars/resources
COPY docker/run_server.sh /water-wars/bin

WORKDIR /water-wars

ENTRYPOINT [ "/water-wars/bin/run_server.sh" ]
