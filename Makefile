CFLAGS=-fPIC
PopCount: popcount.o
	ghc -O2 --make PopCount.hs popcount.o
PopCountTest: PopCount.o popcount.o
	ghc -O2 --make PopCountTest.hs popcount.o
ghci: libpopcount.so
	ghci -lffi -lpopcount PopCount HAMT
clean:
	rm *.o *.hi HAMT IntMapTest *.so
