PDFS=logo_full.pdf logo_circle.pdf logo_circle_notext.pdf
PNGS=logo_full.png logo_circle.png logo_circle_notext.png
HIRESPNGS=logo_full.hires.png logo_circle.hires.png logo_circle_notext.hires.png

.PHONY: clean distclean
all: $(PDFS) $(PNGS) $(HIRESPNGS)

%.pdf: %.tex
	pdflatex $< && pdflatex $< && pdflatex $<
%.png: %.pdf
	convert -density 150 $< $@
	optipng $@
%.hires.png: %.pdf
	convert -density 600 $< $@
	optipng $@

clean:
	rm -f *.aux
	rm -f *.fdb_latexmk
	rm -f *.log
	rm -f *.fls
distclean: clean
	rm -f *.pdf
	rm -f *.png





