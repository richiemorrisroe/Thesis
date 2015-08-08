export TEXINPUTS:=.:./texmf:~/texmf:${TEXINPUTS}
PlaceboMeasurementByMultipleMethods.pdf:
	pdflatex PlaceboMeasurementByMultipleMethods.tex
	pdflatex PlaceboMeasurementByMultipleMethods.tex
HealthforThesis.tex: HealthforThesis.Rnw
	R CMD Sweave HealthforThesis.Rnw
TCQThesis.tex: TCQThesis.Rnw
	R CMD Sweave TCQThesis.Rnw
Experiment.tex: Experiment.Rnw
	R CMD Sweave Experiment.Rnw
Appendix.tex: Appendix.Rnw
	R CMD Sweave Appendix.Rnw
