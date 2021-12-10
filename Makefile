all:
	cabal build
	cabal test

Day01.hs:
	cabal run Aoc2021 1

Day02.hs:
	cabal run Aoc2021 2

Day04.hs:
	cabal run Aoc2021 4

Day05.hs:
	cabal run Aoc2021 5

Day06.hs:
	cabal run Aoc2021 6

Day07.hs:
	cabal run Aoc2021 7

Day08.hs:
	cabal run Aoc2021 8

Day09.hs:
	cabal run Aoc2021 9

Day10.hs:
	cabal run Aoc2021 10

Day11.hs:
	cabal run Aoc2021 11

Day12.hs:
	cabal run Aoc2021 12

Day13.hs:
	cabal run Aoc2021 13

Day14.hs:
	cabal run Aoc2021 14

Day15.hs:
	cabal run Aoc2021 15

Day16.hs:
	cabal run Aoc2021 16

Day17.hs:
	cabal run Aoc2021 17

Day18.hs:
	cabal run Aoc2021 18

Day19.hs:
	cabal run Aoc2021 19

Day20.hs:
	cabal run Aoc2021 20

Day21.hs:
	cabal run Aoc2021 21

Day22.hs:
	cabal run Aoc2021 22

Day23.hs:
	cabal run Aoc2021 23

Day24.hs:
	cabal run Aoc2021 24

Day25.hs:
	cabal run Aoc2021 25

%.hs:
	cabal test

.PHONY: Day01.hs Day03.hs Day04.hs Day05.hs Day06.hs Day07.hs Day08.hs Day09.hs Day10.hs Day11.hs Day12.hs Day13.hs Day14.hs Day15.hs Day16.hs Day17.hs Day18.hs Day19.hs Day20.hs Day21.hs Day22.hs Day23.hs Day24.hs Day25.hs
