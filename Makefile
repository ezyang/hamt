CFLAGS=-fPIC
GHCPROF=-prof -auto-all
GHC=ghc -O2 -fvia-C --make
all: HAMTTest IntMapTest PopCountTest HAMTTest-prof
libpopcount.so: popcount.o
	cc -shared -o libpopcount.so popcount.o
PopCount: popcount.o PopCount.hs
	${GHC} PopCount.hs popcount.o
PopCountTest: popcount.o PopCountTest.hs
	${GHC} PopCountTest.hs popcount.o
IntMapTest: IntMapTest.hs
	${GHC} IntMapTest.hs
IntMapTest-prof: IntMapTest.hs
	${GHC} IntMapTest.hs -o IntMapTest-prof ${GHCPROF}
HAMTTest: popcount.o HAMTTest.hs PopCount.hs
	${GHC} HAMTTest.hs popcount.o
HAMTTest-prof: popcount.o HAMTTest.hs PopCount.hs
	${GHC} HAMTTest.hs -o HAMTTest-prof popcount.o ${GHCPROF}
ghci: libpopcount.so HAMT.hs PopCount.hs
	ghci -lffi -lpopcount HAMT
run-prof: HAMTTest-prof
	./HAMTTest-prof 100000 +RTS -p
run-IntMapTest-prof: IntMapTest-prof
	./IntMapTest-prof 100000 +RTS -p
clean:
	rm -f *.o *.hi *Test *-prof *.so
