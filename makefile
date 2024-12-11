test:
	ghci main.hs

run: build
	./main

build:
	ghc --make main.hs

