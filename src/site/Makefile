all: site favicon.ico css/* images/* posts/* templates/* *.rst *.markdown *.html
	./site build

clear:
	rm -rf _site

site: site.hs
	ghc --make site.hs
