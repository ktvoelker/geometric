
.PHONY: all clean

all: main

main: Main.hs Geometric.hs
	ghc -o main --make Main.hs Geometric.hs

clean:
	-rm *.hi *.o main

