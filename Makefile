all: web-counter

dist/build/web-counter/web-counter: dist/setup-config src/*.hs src/Test/Web/*.hs
	[ ! -f dist/build/web-counter/web-counter ] || rm dist/build/web-counter/web-counter
	cabal build 
	cabal test 

web-counter: dist/build/web-counter/web-counter
	cp dist/build/web-counter/web-counter .

dist/setup-config: web-counter.cabal
	cabal configure --enable-tests
