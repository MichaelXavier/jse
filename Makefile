CABAL = cabal
GHC_PKG = ghc-pkg

all: build

install: install_deps
	$(CABAL) install

uninstall:
	 $(GHC_PKG) unregister jse

build: configure install_deps
	$(CABAL) build

install_deps: jse.cabal
	$(CABAL) install --only-dependencies

configure: jse.cabal *.hs **/*.hs
	$(CABAL) configure

clean:
	$(CABAL) clean
