all: continuations.pdf
	open $<

%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex bibliography.bib
	latexmk -pdf $<

ermodels.lhs: ../packages/Basil/src/Basil/Relations/*.lhs


.PHONY: %.lhs
