all: proposal.pdf
	open $<

proposal.pdf: proposal.lhs

%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex
	bibtex proposal
	pdflatex $<
