GHC	= ghc -odir=build -hidir=build

%:
	$(GHC) $@.hs
