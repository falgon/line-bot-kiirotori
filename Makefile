release:
	@stack build --flag line-bot-kiirotori:release --ghc-options -O2

debug:
	@stack build --fast

.PHONY: release
