postprocessor: *.hs
	ghc -O2 -dynamic Main.hs -o postprocessor

.PHONY: clean
clean:
	rm -f **.o **.hi postprocessor
