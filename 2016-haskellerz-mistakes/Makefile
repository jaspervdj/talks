.PHONY: clean

slides.pdf: slides.tex \
		images/strings.pdf
	xelatex $<

images/strings.pdf: images/strings.dot
	dot -Tpdf images/strings.dot >images/strings.pdf

clean:
	rm -f slides.pdf
	rm -f images/*.pdf
