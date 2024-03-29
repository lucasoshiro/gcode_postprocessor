FLAGS=-O2 -dynamic
GHC=ghc ${FLAGS}

.PHONY: all
all: postprocessor gcodeinfo gcodetranslate

postprocessor: *.hs
	${GHC} PostProcessor.hs -o $@

gcodeinfo: *.hs
	${GHC} GCodeInfo.hs -o $@

gcodetranslate: *.hs
	${GHC} Translate.hs -o $@

.PHONY: clean
clean:
	rm -f **.o **.hi postprocessor gcodeinfo gcodetranslate
