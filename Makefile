CFLAGS=-fPIC
GHCPROF=-prof -auto-all
libpopcount.so: popcount.o
	cc -shared -o libpopcount.so popcount.o
PopCount: popcount.o
	ghc -O2 --make PopCount.hs popcount.o
PopCountTest: popcount.o
	ghc -O2 --make PopCountTest.hs popcount.o
IntMapTest:
	ghc -O2 --make IntMapTest.hs
IntMapTest-prof:
	ghc -O2 --make IntMapTest.hs -o IntMapTest-prof ${GHCPROF}
HAMTTest: popcount.o
	ghc -O2 --make HAMTTest.hs popcount.o
HAMTTest-prof: popcount.o
	ghc -O2 --make HAMTTest.hs -o HAMTTest-prof popcount.o ${GHCPROF}
ghci: libpopcount.so
	ghci -lffi -lpopcount HAMT
run-prof: clean HAMTTest-prof
	./HAMTTest-prof 1000000 +RTS -p
run-IntMapTest-prof: clean IntMapTest-prof
	./IntMapTest-prof 1000000 +RTS -p
all: HAMTTest IntMapTest PopCountTest HAMTTest-prof
clean:
	rm -f *.o *.hi *Test *-prof *.so
