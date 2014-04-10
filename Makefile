
all:
	cabal build

doc:
	cabal haddock

conf:
	cabal configure

depinstall:
	cabal install -j4 --only-dependencies

