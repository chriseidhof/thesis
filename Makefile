all: rewrite.pdf
	open $<

rewrite.pdf: rewrite.lhs

%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex
	bibtex proposal
	latexmk -pdf $<
