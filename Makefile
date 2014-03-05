all: 
	mkdir -p dist
	cd src/daemon && make && cp dist/build/daemon/daemon ../../dist
	cd src/site && make && cp -R _site/* ../../dist

clear:
	rm -rf dist
	cd src/daemon && make clear
	cd src/site && make clear

run:
	cd dist && ./daemon
