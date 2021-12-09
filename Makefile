release:
	@stack build --flag line-bot-kiirotori:release --ghc-options -O2

debug:
	@stack build --fast

restyle:
	@find ./app ./src -type f -name "*.hs" | xargs -n1 stylish-haskell -i

restyle-with-hlint: restyle
	hlint ./app ./src

.PHONY: release debug restyle restyle-with-hlint
