postprocessor: *.hs
	ghc -dynamic Main.hs -o postprocessor

.PHONY: clean
clean:
	rm -f **.o **.hi postprocessor
