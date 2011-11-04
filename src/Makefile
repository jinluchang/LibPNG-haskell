all : libpng-haskell-example
	./libpng-haskell-example

libpng-haskell-example : Main.hs
	ghc -o libpng-haskell-example -lpng -Wall -O2 --make Main.hs

clean :
	rm libpng-haskell-example *.o *.hi *.png Codec/Image/*.hi Codec/Image/*.o
