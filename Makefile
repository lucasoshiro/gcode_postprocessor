postprocessor: *.hs
	ghc -dynamic gcode_postprocessor.hs -o postprocessor

.PHONY: clean
clean:
	rm -f **.o **.hi postprocessor
