#all: site favicon.ico css/* images/* posts/* templates/* *.rst *.markdown *.html
#	./site build
all: html_old/*
	rm -rf _site
	cp -R html_old _site

clear:
	rm -rf _site
	rm -rf _cache
	rm site

site: site.hs
	ghc --make site.hs
