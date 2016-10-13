all: _shake/build
	_shake/build --report

_shake/build: Build.hs
	mkdir -p _shake
	stack ghc -- --make Build.hs src-build/PNLPipeline.hs -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/build
