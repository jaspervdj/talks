default: hellofib.eventlog binary-trees.prof.html binary-trees.pdf

hellofib.eventlog: hellofib
	./hellofib +RTS -N2 -l

hellofib: hellofib.hs
	stack exec ghc -- -O2 -rtsopts -eventlog -threaded hellofib

binary-trees.prof.html: binary-trees.prof
	profiteur binary-trees.prof

binary-trees.pdf: binary-trees.prof
	hp2ps -c binary-trees.hp
	ps2pdf binary-trees.ps

binary-trees.prof: binary-trees
	./binary-trees 15 +RTS -p -hy -s -RTS 2>binary-trees.stderr

binary-trees:
	stack exec ghc -- --make -auto-all -prof -rtsopts binary-trees.hs

clean:
	rm -f *.hi
	rm -f *.o
	rm -f binary-trees
	rm -f binary-trees.aux
	rm -f binary-trees.hp
	rm -f binary-trees.pdf
	rm -f binary-trees.prof
	rm -f binary-trees.prof.html
	rm -f binary-trees.ps
	rm -f binary-trees.stderr
	rm -f hellofib
	rm -f hellofib.eventlog

.PHONY: clean
