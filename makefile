OPTIMIZATION=-O0
build: 
	cabal new-build all -j --ghc-options $(OPTIMIZATION)

haddock:
	cabal new-haddock all

haddock-hackage:
	cabal new-haddock all --haddock-for-hackage --haddock-option=--hyperlinked-source
	echo "the hackage ui doesn't accept the default format, use command instead"
	cabal upload -d --publish ./dist-newstyle/*-docs.tar.gz

ghcid: clean etags
	ghcid \
		--test="main" \
		--command="ghci" \
		test/Spec
ghcid-app: clean etags
	ghcid \
		--main="main" \
		--command="ghci" \
		app/exe

ghci:
	ghci app/exe

etags:
	hasktags  -e ./src

clean:
	rm -fR dist dist-*
	find . -name '*.hi' -type f -delete
	find . -name '*.o' -type f -delete
	find . -name '*.dyn_hi' -type f -delete
	find . -name '*.dyn_o' -type f -delete
	find . -name 'autogen*' -type f -delete

.PHONY: test

sdist: hpack
	nix-build . # ad-hoc proof it builds
	cabal sdist

run:
	cabal new-run game13 --ghc-options $(OPTIMIZATION) -- \

brittany_:
	$(shell set -x; for i in `fd hs`; do hlint --refactor --refactor-options=-i $$i; brittany --write-mode=inplace $$i; done)

brittany:
	nix-shell ./nix/travis-shell.nix --run "make brittany_"

bundle:
	rm -f result
	nix-build nix/bundle.nix
	mv result template

test:
	cabal new-test --ghc-options "-O0"
