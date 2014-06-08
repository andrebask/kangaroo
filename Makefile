CC = gcc
OPTS = -no-user-package-db -package-db cabal-dev/packages-*.conf

all:
	$(CC) -fPIC -shared includes/C/cbits.c -o dist/build/kangaroo/cbits.so
	ghc $(OPTS) dist/build/kangaroo/cbits.so --make src/*.hs -o kangaroo

clean:
	rm kangaroo dist/build/kangaroo/* src/*.hi src/*.o
