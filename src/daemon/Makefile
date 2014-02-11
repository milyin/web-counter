all: dist/build/web-counter/web-counter

clear:
	rm -rf dist

dist/build/web-counter/web-counter: dist/setup-config src/*.hs src/Test/Web/*.hs
	[ ! -f dist/build/web-counter/web-counter ] || rm dist/build/web-counter/web-counter
	cabal build 
	cabal test 

dist/setup-config: web-counter.cabal
	cabal configure --enable-tests
