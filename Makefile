all: 
	mkdir -p dist
	cd src/web-counter && make && cp dist/build/web-counter/web-counter ../../dist
	cd src/site && make && cp -R _site/* ../../dist

clear:
	rm -rf dist
	cd src/web-counter && make clear
	cd src/site && make clear

