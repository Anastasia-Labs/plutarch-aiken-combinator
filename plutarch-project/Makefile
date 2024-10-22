.PHONY: help
help:
	@echo "use: make <command>"
	@echo
	@echo "Where <command>:"
	@echo "  help           -- show this help"
	@echo "  export         -- exports the contracts to the exported directory and aiken env"
	@echo "  export_testnet -- exports the unoptimized contracts to the exported directory and aiken env"

.PHONY: export
export:
	cabal run plutarch-project

.PHONY: export_testnet
export_testnet:
	cabal run plutarch-project -f testnet

