thread:
	ghc -threaded -O2 -Wall -fno-warn-unused-do-bind -o logreply Main.hs
nonthread:
	ghc -O2 -Wall -fno-warn-unused-do-bind -o logreply Main.hs

clean:
	rm -f *.hi *.o logreply
