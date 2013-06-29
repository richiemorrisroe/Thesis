ThesisContents030511.pdf: ThesisContents030511.tex LitReviewThesis050211.pdf Methodologyplan120211.pdf HealthforThesis.pdf TCQThesis.pdf TheoryPlacebo.pdf Introduction.pdf Experiment.pdf DiscussionandConclusions.pdf  
LitReviewThesis.pdf: LitReviewThesis.tex
	pdflatex LitReviewThesis.tex
	pdflatex LitReviewThesis.tex
Methodologyplan120211.pdf: Methodologyplan120211.tex
	R CMD pdflatex Methodologyplan120211.tex
	R CMD pdflatex Methodologyplan120211.tex
HealthforThesis.pdf: HealthforThesis.tex
	R CMD Sweave HealthforThesis.Rnw
	R CMD pdflatex HealthforThesis.tex
	R CMD pdflatex HealthforThesis.tex
TCQThesis.pdf: TCQThesis.tex
	R CMD Sweave TCQThesis.Rnw
	R CMD pdflatex TCQThesis.tex
	R CMD pdflatex TCQThesis.tex
Experiment.pdf: Experiment.tex
	R CMD Sweave Experiment.Rnw
	R CMD pdflatex Experiment.tex
	R CMD pdflatex Experiment.tex
TheoryPlacebo.pdf: TheoryPlacebo.tex
	R CMD Sweave TheoryPlacebo.tex
	R CMD pdflatex TheoryPlacebo.tex
	R CMD pdflatex TheoryPlacebo.tex
Introduction.pdf: Introduction.tex
	R CMD pdflatex Introduction.tex
	R CMD pdflatex Introduction.tex
DiscussionandConclusions.pdf: DiscussionandConclusions.tex
	R CMD pdflatex DiscussionandConclusions.tex
	R CMD pdflatex DiscussionandConclusions.tex
ThesisContents030511.odt: ThesisContents030511.odt LitReviewThesis050211.odt Methodologyplan120211.odt HealthforThesis.odt TCQThesis.odt TheoryPlacebo.odt Introduction.odt Experiment.odt DiscussionandConclusions.odt  
	LitReviewThesis.odt: LitReviewThesis.tex
		pdflatex LitReviewThesis.tex
		pdflatex LitReviewThesis.tex
		oolatex LitReviewThesis.tex
	Methodologyplan120211.odt: Methodologyplan120211.tex
		pdflatex Methodologyplan120211.tex
		pdflatex Methodologyplan120211.tex
		oolatex Methodologyplan120211.tex
	HealthforThesis.odt: HealthforThesis.tex
		R CMD Sweave HealthforThesis.Rnw
		R CMD pdflatex HealthforThesis.tex
		R CMD pdflatex HealthforThesis.tex
		R CMD oolatex HealthforThesis.tex
	TCQThesis.odt: TCQThesis.tex
		R CMD Sweave TCQThesis.Rnw
		R CMD pdflatex TCQThesis.tex
		R CMD pdflatex TCQThesis.tex
		R CMD oolatex TCQThesis.tex
	Experiment.odt: Experiment.tex
		R CMD Sweave Experiment.Rnw
		R CMD pdflatex Experiment.tex
		R CMD pdflatex Experiment.tex
		R CMD oolatex Experiment.tex
	TheoryPlacebo.odt: TheoryPlacebo.tex
		R CMD Sweave TheoryPlacebo.tex
		R CMD pdflatex TheoryPlacebo.tex
		R CMD pdflatex TheoryPlacebo.tex
		R CMD oolatex TheoryPlacebo.tex
	Introduction.odt: Introduction.tex
		R CMD pdflatex Introduction.tex
		R CMD pdflatex Introduction.tex
		R CMD oolatex Introduction.tex
	DiscussionandConclusions.odt: DiscussionandConclusions.tex
		R CMD pdflatex DiscussionandConclusions.tex
		R CMD pdflatex DiscussionandConclusions.tex
		R CMD oolatex DiscussionandConclusions.tex