all: ermodels.pdf
	open $<

%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex
	latexmk -pdf $<

ermodels.lhs: ../packages/Basil/src/Basil/Relations/*.lhs


.PHONY: %.lhs
