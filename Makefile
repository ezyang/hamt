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
HAMTTest: popcount.o
	ghc -O2 --make HAMTTest.hs popcount.o
HAMTTest-prof: popcount.o
	ghc -O2 --make HAMTTest.hs -o HAMTTest-prof popcount.o ${GHCPROF}
ghci: libpopcount.so
	ghci -lffi -lpopcount HAMT
run-prof: HAMTTest-prof
	./HAMTTest-prof 100000 +RTS -K1G -p
all: HAMTTest IntMapTest PopCountTest HAMTTest-prof
clean:
	rm -f *.o *.hi *Test *-prof *.so
