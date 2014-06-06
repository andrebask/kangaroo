CC = gcc
OPTS = -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d

all:
	$(CC) -fPIC -shared includes/C/cbits.c -o dist/build/kangaroo/cbits.so
	ghc $(OPTS) dist/build/kangaroo/cbits.so --make src/*.hs -o kangaroo

