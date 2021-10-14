postprocessor: *.hs
	ghc -O2 -dynamic PostProcessor.hs -o postprocessor

.PHONY: clean
clean:
	rm -f **.o **.hi postprocessor
