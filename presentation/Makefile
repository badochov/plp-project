

generuoti: slides.pdf

.SECONDEXPANSION:
%.pdf:  %.tex $$(wildcard build/%.aux build/%.bbl build/%.toc) bibliography.bib
	lualatex --halt-on-error --output-directory=build/ $<
	cp -a build/$@ .
	biber build/$*.bcf