all: l3tolib

l3tolib: l3tolib.lhs
	ghc $^ -o $@

.PHONY: clean mrproper

clean:
	rm -f l3tolib.hi l3tolib.o

mrproper: clean
	rm -f l3tolib
