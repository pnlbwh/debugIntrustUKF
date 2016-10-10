all: _shake/build
	_shake/build --report

_shake/build: src/Build.hs
	mkdir -p _shake
	stack ghc -- --make src/Build.hs -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/build
