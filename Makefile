all : haskell-example
	./haskell-example
	eog Water.png

haskell-example : Main.hs
	ghc -o haskell-example -lpng -Wall -O2 --make Main.hs

clean :
	rm haskell-example example *.o *.hi *-copy.png

clean-png :
	rm *.png
