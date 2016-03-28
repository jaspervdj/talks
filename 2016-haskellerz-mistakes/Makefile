.PHONY: clean

slides.pdf: slides.tex \
		images/strings.pdf \
		images/strings-lazy.pdf
	xelatex $<

images/strings.pdf: images/strings.dot
	dot -Tpdf $< >$@

images/strings-lazy.pdf: images/strings-lazy.dot
	dot -Tpdf $< >$@

clean:
	rm -f slides.pdf
	rm -f images/*.pdf
