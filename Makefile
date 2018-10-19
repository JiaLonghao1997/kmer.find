all: haskell bin/sort32pairs

haskell:
	mkdir -p bin
	stack install --local-bin-path $$PWD/bin

bin/sort32pairs: src/sort32pairs.cpp
	g++ -O2 -o $@ $^
