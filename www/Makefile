.PHONY: wasm web clean deploy deploy-production

JS_SOURCES = $(shell find . | egrep -v 'node_modules|\.next|out|\.next-env\.d\.ts' | egrep '\.js$$|\.ts$$|\.tsx$$')

web: out/index.html
wasm: public/boolean-www.js

out/index.html: public/boolean-www.js node_modules $(JS_SOURCES)
	yarn run --silent next build
	yarn run --silent next export

deploy: out/index.html node_modules
	yarn run --silent netlify deploy --dir="out"

deploy-production: out/index.html node_modules
	yarn run --silent netlify deploy --dir="out" --prod

public/boolean-www.js: 
	cargo web build --release
	cp ../target/wasm32-unknown-unknown/release/boolean-www.js public/boolean-www.js
	cp ../target/wasm32-unknown-unknown/release/boolean-www.wasm public/boolean-www.wasm

node_modules: package.json yarn.lock
	yarn install --dev --frozen-lockfile
	touch node_modules

clean:
	rm -rf ".next" "node_modules" "out" "target" "public/boolean-www.js" "public/boolean-www.wasm"

example: 
	echo "$(JS_SOURCES)"
