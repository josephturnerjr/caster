prep:
	wget https://www.stackage.org/snapshot/lts-1.15/cabal.config?download=true
	cabal update
	cabal sandbox init
	cabal install --only-dependencies
