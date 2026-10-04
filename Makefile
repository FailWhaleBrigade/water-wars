.PHONY= update build optim

CABAL_ARGS=--with-compiler=wasm32-wasi-ghc-9.14 --with-hc-pkg=wasm32-wasi-ghc-pkg-9.14 --with-hsc2hs=wasm32-wasi-hsc2hs-9.14 --with-haddock=wasm32-wasi-haddock-9.14

all: update build optim

js: update-js build-js

js86: configure-js86 update-js86 build-js86

update:
	cabal update $(CABAL_ARGS) --project-file cabal.wasm.project

build:
	cabal build  $(CABAL_ARGS) --project-file cabal.wasm.project exe:water-wars-client
	rm -rf public
	cp -r static public
	cp -r resources/textures public/textures
	$(eval my_wasm=$(shell cabal list-bin $(CABAL_ARGS) --project-file cabal.wasm.project exe:water-wars-client | tail -n 1))
	$(shell wasm32-wasi-ghc-9.14 --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/water-wars-client.wasm -o public/water-wars-client.wasm
	wasm-tools strip -o public/water-wars-client.wasm public/water-wars-client.wasm

watch:
	ghciwatch \
		--after-startup-ghci :main \
		--after-reload-ghci :main \
		--watch *.hs --debounce 50ms \
		--command 'cabal repl $(CABAL_ARGS) --project-file cabal.wasm.project exe:water-wars-client -finteractive --repl-options="-fghci-browser -fghci-browser-port=8000"'

serve:
	simple-http-server --nocache public --open --index

repl:
	cabal repl $(CABAL_ARGS) --project-file cabal.wasm.project exe:water-wars-client -finteractive --repl-options='-fghci-browser -fghci-browser-port=8000'

clean:
	rm -rf ../dist-newstyle public
