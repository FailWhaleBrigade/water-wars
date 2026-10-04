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
    patch

RUN BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH=/root/.ghcup/bin:$PATH


# Wasm specific tools
# See https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta
RUN apt install -y \
    binaryen \
    wasm-tools \
    emscripten \
    jq \
    unzip \
    zstd

# https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta#using-ghcup
RUN curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | SKIP_GHC=1 sh \
    && . /root/.ghc-wasm/env \
    && ghcup config add-release-channel https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/ghcup-wasm-0.0.9.yaml \
    && ghcup install ghc wasm32-wasi-9.14 -- $CONFIGURE_ARGS

COPY client/ /water-wars/client
COPY library/ /water-wars/library
COPY client/ /water-wars/server
COPY resources/ water-wars/resources
COPY static/ water-wars/static
COPY cabal.project cabal.wasm.project Makefile /water-wars/
