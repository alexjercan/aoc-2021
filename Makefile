all:
	cabal build
	cabal test

Day%Test.hs:
	cabal test

Day%.hs:
	cabal run Aoc2021 $* < input/day$*.input

%.hs:
	cabal test

%:
	cabal run Aoc2021 $* < input/day$(shell printf '%02d' $*).input
