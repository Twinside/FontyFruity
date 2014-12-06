
all:
	cabal build

doc:
	cabal haddock

conf:
	cabal configure

depinstall:
	cabal install -j4 --only-dependencies

lint:
	hlint lint src --cpp-define=MIN_VERSION_binary=1
