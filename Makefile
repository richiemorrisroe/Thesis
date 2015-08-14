export TEXINPUTS:=.:./texmf:~/texmf:${TEXINPUTS}
PlaceboMeasurementByMultipleMethods.pdf: HealthforThesis.tex TCQThesis.tex Experiment.tex Appendix.tex DevIATQuant.tex
	pdflatex PlaceboMeasurementByMultipleMethods.tex
	pdflatex PlaceboMeasurementByMultipleMethods.tex
HealthforThesis.tex: HealthforThesis.Rnw
		R CMD Sweave HealthforThesis.Rnw
TCQThesis.tex: TCQThesis.Rnw
		R CMD Sweave TCQThesis.Rnw
Experiment.tex: Experiment.Rnw TCQThesis.Rnw HealthforThesis.Rnw
		R CMD Sweave Experiment.Rnw
Appendix.tex: Appendix.Rnw
		R CMD Sweave Appendix.Rnw
DevIATQuant.tex: DevIATQuant.Rnw
		R CMD Sweave DevIATQuant.Rnw
clean:
	rm HealthforThesis.tex TCQThesis.tex Experiment.tex Appendix.tex DevIATQuant.tex
