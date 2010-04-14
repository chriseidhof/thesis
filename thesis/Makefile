all: conclusion.pdf
	open $<

%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex bibliography.bib
	latexmk -pdf $<

ermodels.tex: ../packages/Basil/src/Basil/Relations/*.lhs ermodels/*.lhs

continuations.tex: continuations/*.lhs

quiz.tex: quiz/*.lhs

.PHONY: %.lhs
