all: dist/build/daemon/daemon

clear:
	rm -rf dist

dist/build/daemon/daemon: dist/setup-config src/*.hs src/Test/Web/*.hs
	[ ! -f dist/build/daemon/daemon ] || rm dist/build/daemon/daemon
	cabal build 
	cabal test 

dist/setup-config: daemon.cabal
	cabal configure --enable-tests
