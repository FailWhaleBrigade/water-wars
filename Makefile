.PHONY= update build optim

all: update build optim

js: update-js build-js

js86: configure-js86 update-js86 build-js86

update:
	wasm32-wasi-cabal update

build:
	wasm32-wasi-cabal build exe:water-wars-client
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin exe:water-wars-client | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/water-wars-client.wasm -o public/water-wars-client.wasm
	wasm-tools strip -o public/water-wars-client.wasm public/water-wars-client.wasm

watch:
	ghciwatch --after-startup-ghci :main --after-reload-ghci :main --watch *.hs --debounce 50ms --command 'wasm32-wasi-cabal repl exe:water-wars-client -finteractive --repl-options="-fghci-browser -fghci-browser-port=8080"'

serve:
	simple-http-server --nocache public --open --index

repl: update
	wasm32-wasi-cabal repl exe:water-wars-client -finteractive --repl-options='-fghci-browser -fghci-browser-port=8080'

clean:
	rm -rf ../dist-newstyle public
