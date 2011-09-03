GHC = ghc
GHC_OPTS = -O3

all: bin/jse

clean:
	rm -f *.hi *.o JSE/*.hi JSE/*.oa bin/jse

bin:
	mkdir bin

bin/jse: Main.hs JSE.hs JSE/*.hs bin
	$(GHC) $(GHC_OPTS) -o $@ $<
	chmod +x $@
