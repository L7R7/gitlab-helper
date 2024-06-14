install:
	stack build --copy-bins

check: format lint

lint:
	hlint src/ test/ app/

weed:
	rm -rf .hie
	stack clean
	stack test --no-run-tests
	weeder

format:
	ormolu --mode check $(shell find src/ test/ app/ -type f -name "*.hs")

reformat:
	ormolu --mode inplace $(shell find src/ test/ app/ -type f -name "*.hs")

build:
	stack clean
	stack test --pedantic

deploy: build check weed
	git push

loc: production-loc test-loc total-loc

production-loc:
	$(info production code)
	tokei -e test/ -e test_resources/ -t=Haskell

test-loc:
	$(info test code)
	tokei -e src/ -e app/ -e teams-responder-client/ -t=Haskell,JSON

total-loc:
	$(info total)
	tokei
