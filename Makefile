all : haskell-example
	./haskell-example
	eog spectrum.png

haskell-example : Main.hs
	ghc -o haskell-example -lpng -Wall -O2 --make Main.hs

example : libpng-short-example.c
	gcc -o example -Wall -lpng libpng-short-example.c

clean :
	rm haskell-example example *.o *.hi *.png
