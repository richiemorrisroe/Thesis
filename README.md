#The Placebo Effect: Measurement By Multiple Methods

This is my thesis, titled as above.

It attempts to be a reasonably comprehensive record of the body of work that I submitted to UCC in fulfillments of the requirements of a Doctorate. Specifically, its a doctorate in Applied Psychology.

Don't let that fool you though, it's an awful lot of code.

Primarily, this repository consists of the LaTeX and R code required to build my thesis. It was written and run on a number of Linux machines (4-5) over the course of a number of years. Some of the text was written before that, as appears in the initial commit.

Unfortunately, I didn't discover Git at all until around January 2012 (https://github.com/richiemorrisroe/Thesis/commit/161ea9cba749e7ffb2496db6517c8387620f7eab) but the commit message claims to be a recommit of all files.

As you can probably tell, I didn't really know what I was doing with respect to Git.

Nonetheless, this repository contains my additions, deletions and changes for a pretty substantial amount of work (check out all the code I commented out, or put in `eval=FALSE` blocks).

I hope that this can prove useful to someone, somewhere. I hope to tidy up and generalise much of the R Code (here: https://github.com/richiemorrisroe/thesisR), and you're welcome to follow along.

I'll probably grab some of this code and text for future projects, and I'd appreciate if you could treat my text under a Creative-Commons Attribution License, and my code under the GPL-V3). Mostly, though, this repository is done, along with the thesis.

If anyone finds errors or omissions in this, please send a pull request. 

#A note on building

Currently, this won't work.
The document can be generated (you'll need to install `texlive-full` on Ubuntu or similar, and install a number of R packages), but there are lots of text files which are not generated as part of the build which should be (the python scripts which generate the IAT data are present, but not used).

Also, I'd like you to consider that this was the project with which I learned to code, and that there are many terrible hacks, repeated code, imprecise errors, unwarranted assumptions and clever hacks. In this, it is like many PhD's :) (i hope). 