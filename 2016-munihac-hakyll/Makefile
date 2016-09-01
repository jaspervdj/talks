.PHONY: clean

slides.pdf: slides.tex \
		images/deps-01.dot.pdf \
		images/deps-02.dot.pdf \
		images/deps-03.dot.pdf \
		images/deps-04.dot.pdf \
		images/deps-05.dot.pdf \
		images/deps-06.dot.pdf
	xelatex -halt-on-error $<

images/%.dot.pdf: images/%.dot
	dot -Tpdf $< >$@

clean:
	rm -f slides.pdf
	rm -f images/*.dot.pdf
