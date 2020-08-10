# this file was tested using GNUMAKE >= 4.2.1.

# this is necessary for using multi-line strings as command arguments.
SHELL := $(shell which bash)

# this allows omitting newlines.
.ONESHELL:

# "nix-shell -p" constructs an expression that relies on <nixpkgs> for
# selecting attributes, so we override it.
# https://github.com/NixOS/nix/issues/726#issuecomment-161215255
NIX_PATH := nixpkgs=./.

.PHONY: all
all: nixfmt shellcheck ormolu hlint api

#STYLE

.PHONY: nixfmt
nixfmt:
	@nix-shell --pure -p fd nixfmt --run bash <<< '
		RETURN=0
		for F in $$(fd -e nix); do
			nixfmt -c $$F
			if [ $$? -ne 0 ]; then
				echo "[!] $$F does not pass nixfmt format check. Formatting.." >&2
				nixfmt $$F
				RETURN=1
			fi
		done
		if [ $$RETURN -ne 0 ]; then exit 1; fi
	'
	

.PHONY: shellcheck
shellcheck:
	@nix-shell --pure -p fd shellcheck --run bash <<< '
		for F in $$(fd -e sh); do
			shellcheck -s bash $$F
		done
	'



.PHONY: ormolu
ormolu:
	@nix-shell --pure -E '
		let pkgs = import <nixpkgs> {};
		in pkgs.mkShell {
			buildInputs = [pkgs.fd pkgs.ormolu];
			shellHook =
				"export LOCALE_ARCHIVE=$${pkgs.glibcLocales}/lib/locale/locale-archive \n" +
				"export LANG=en_US.UTF-8";
		}
	' --run bash <<< '
		RETURN=0
		for F in $$(fd -e hs); do
			ormolu -o -XTypeApplications -o -XPatternSynonyms -m check $$F
			if [ $$? -ne 0 ]; then
				echo "[!] $$F does not pass ormolu format check. Formatting.." >&2
				ormolu -o -XTypeApplications -o -XPatternSynonyms -m inplace $$F
				RETURN=1
			fi
		done
		if [ $$RETURN -ne 0 ]; then exit 1; fi
	'

.PHONY: hlint
hlint:
	@nix-shell --pure -p hlint --run bash <<< '
		hlint client/ --hint=./.hlint.yaml
		hlint common/ --hint=./.hlint.yaml
		hlint server/ --hint=./.hlint.yaml
	'

.PHONY:api
api:
	nix-shell --run 'cabal v2-run docs'
