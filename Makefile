all:
	cabal build
	cabal test

Day%.hs:
	cabal run Aoc2021 $*

%.hs:
	cabal test

