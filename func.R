##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x
##' @param ...
##' @return
##' @author Richard Morrisroe
FactorXtab <-  function (x, names=NULL,  ...) {
    x.mat.df <- FactorCoeff(x, names=names)
    x.mat.df[] <- sapply(x.mat.df, function (x) round(x, digits=2))
    getfacs <- dim(x.mat.df)[2]-1
    x.mat.df[,1:getfacs] <- sapply(x.mat.df[,1:getfacs], function (x) ifelse(x>=0.3, paste0("\\bftab ",  x), x))
    
    fact.xtab <- xtable(x.mat.df,...)
    align(fact.xtab) <- rep("r", ncol(fact.xtab)+1)
    return(fact.xtab)
}
FactorCoeff <- function (x, names=NULL) {
   x.load<-x$loadings
    x.comm<-x$communality
    x.names <- colnames(x.load)
    len <- length(colnames(x.load))
   
    x.comm.load<-cbind(x.load, x.comm)

   x.mat.df<-as.matrix.data.frame(x.comm.load)
   if(!is.null(names)) {
       names2 <- c(names, "Communalites")
       colnames(x.mat.df) <- names2
   }
   else {
       x.names[len+1] <- "Communalities"
       colnames(x.mat.df) <- x.names

   }
   return(x.mat.df)
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param x
##' @param ...
##' @return
##' @author Richard Morrisroe
FactorCor <- function (x, xtable=FALSE, names=NULL, ...) {
  res <- x$r.scores
  #allnames <- attr(x$loadings, "dimnames")
  if(is.null(names)) {
  factnames <- colnames(x$loadings)
}
  else {
      factnames <- names
  }
  ## browser()
  res <- as.data.frame(res)
  names(res) <- factnames
  rownames(res) <- factnames
  #names(res) <- factnames
  if(xtable) {
  res <- xtable(res, ...)
}
  res
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param x
##' @param loadings
##' @return
##' @author Richard Morrisroe
ExtractLoadings <- function (x, loadings=0.3) {
  x.load <- x$loadings
  x.uc.mat <- as.data.frame(unclass(x.load))
  xitemsload <- apply(x.uc.mat, 2, function (y) names(y[which(abs(y)>=loadings)])) #return the names of items which have appreciable loadings
  xitemsload
}
## FactorCor <- function (x) {
##   cor <- x$r.scores
##  xtable(cor)
## }
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param x
##' @return
##' @author Richard Morrisroe
Extracth2u2 <- function (x) {
  x.comm <- x$communality
  x.uniq <- x$uniquenesses
  x.ratio <- x.comm/x.uniq
  x.h2u2 <- as.data.frame(cbind(x.comm, x.uniq, x.ratio))
  x.h2u2
 }
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param x
##' @param ...
##' @return
##' @author Richard Morrisroe
IrtXtab <- function (x, ...) {
  eta<-x$etapar #$
  se<-x$se.eta #$
  eta.mat<-as.matrix(eta)
  se.eta.mat<-as.matrix(se)
  eta.par.mat<-cbind(eta.mat, se.eta.mat)
  colnames(eta.par.mat) <- c("Ability Estimate", "Standard Error")
  coef.xtab<-xtable(eta.par.mat, ...)
  coef.xtab
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param x
##' @return
##' @author Richard Morrisroe
coefreshape <-  function (x) {
#  mxlength <- lapply(x, max.length)
  res <- matrix(NA, ncol=length(x), nrow=10)
  rownames(x) <- names(x[3])
  for (i in seq_along(x)) {
    y <- get(x[i])
    res[,i] <- y[]
  }
  res
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param x
##' @return
##' @author Richard Morrisroe
FitIndices <- function (x, labels=NULL) {
  tli <- x$TLI
  bic <- x$BIC
  rmsea <- x$RMSEA
  labels <-
  rmsnames <- attr(x$RMSEA, "names")
  res <- as.data.frame(cbind(tli, bic, rmsea[1], rmsea[2], rmsea[3]))
  print(length(res))
  colnames(res) <- c(paste(substitute(x),"TLI", sep=""),paste(substitute(x),"BIC", sep=""),
                     paste(substitute(x),"RMSEA", sep=""),
                     paste(substitute(x),"-90CInt", sep=""),
                     paste(substitute(x),"+90CInt", sep=""))
  res
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param data
##' @param factors
##' @param meth
##' @param rotation
##' @param scores
##' @return
##' @author Richard Morrisroe
MultFactorAnalysis <- function (data, factors, meth, rotation, scores) {
  orthrotations <- c("none", "varimax", "quartimax", "bentlerT", "geominT" )
  obliquerotations <- c("promax", "oblimin",
          "simplimax", "bentlerQ", "geominQ", "biquartimin")
  allrot <- c(orthrotations, obliquerotations)

  meth <- c("minres", "wls", "gls", "pa", "ml")
  Scores <- c("regression", "Thurstone", "Anderson", "Bartlett", "tenBerge")
  fno <- factors
  rotlist <- list()
  for (i in seq_along(along.with=allrot)) {
   x <- fa(na.omit(data), nfactors=fno, rotate=allrot[i], fm="ml")
  assign(paste("rot", i,sep=""), value=x)

   rotlist[[i]] <- get(paste("rot", i, sep=""))

}
  names(rotlist) <- allrot
  reslist <- rotlist

  fmlist <- list()
  for (j in seq_along(along.with=meth)) {
    y <- fa(na.omit(data), nfactors=fno, rotate="oblimin", fm=meth[j])
    assign(paste("meth", j, sep=""), value=y)
    fmlist[[j]] <- get(paste("meth", j, sep=""))
}
  names(fmlist) <- meth
  res <- c(factormethods=fmlist, rotations=reslist)

}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param mfa
##' @return
##' @author Richard Morrisroe
getLoadings <- function (mfa) {
  ind <- lapply(mfa, ExtractLoadings)
  ind
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param mfa
##' @return
##' @author Richard Morrisroe
combineLoadings <-  function (mfa) {
  loadlist <- list()
  for (i in seq_along(along.with=mfa)) {
    loadlist[[i]] <- mfa[[i]]$loadings

  }
  loadlist
  loadings <- list()
  for (j in seq_along(along.with=loadlist)) {


    loadings[[j]] <- as.matrix(unclass(loadlist[[j]]))
  }
  loadings

  meanload <- lapply(loadings, function (x) Reduce('+', x))
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param mfa
##' @param method
##' @param rotreq
##' @return
##' @author Richard Morrisroe
displayRot <- function (mfa, method=NULL, rotreq=NULL) {
  rotationreq <- rotreq
  meth <- method
  resind <- grep(rotationreq, x=names(mfa))
  res <- mfa[[resind]]
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param grm
##' @param ...
##' @return
##' @author Richard Morrisroe
ggplotGRM <- function (grm, ...) {
  x <- coef(grm)
  x <- as.matrix(x)
  x <- x[,-ncol(x)]
  x.t <- t(x)
  response <- 1:nrow(x.t)
  respind <- ncol(x.t)+1
  x.t <- as.data.frame(x.t)
  x.t$response <- response
  x.tm <- melt(x.t, id="response")
  names(x.tm) <- c("threshold", "item", "ability")
  plot1 <- ggplot(x.tm, aes(x=ability, y=item, shape=as.factor(threshold), colour=as.factor(threshold)), ...)
  plot2 <- plot1+geom_point()+geom_rug()
  plot2
  }
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param gpcm
##' @return
##' @author Richard Morrisroe
coef2mat <- function (gpcm) {
  if(is.matrix(gpcm)) {
    return(gpcm)
  }
  else {

    len <- lapply(gpcm, length)
    probelem <- which.min(as.matrix(unlist(len)))
    dimcols <- max(as.matrix(unlist(len)))
    dimrows <- length(names(gpcm))
    mat.res <- matrix(NA, nrow=dimrows, ncol=dimcols)
    modlength <- lapply(gpcm, length)
    maxlength <- max(as.matrix(unlist(modlength)))
    for (i in 1:maxlength) {
      column <- lapply(gpcm, "[", i)
      column <- as.matrix(unlist(column))
      ## browser()
      mat.res[1:length(column),i] <- column
      mat.res
    }
    rownames(mat.res) <- names(gpcm)
    probelemlength <- length(gpcm[[probelem]])
    ## browser()
    missingvalue <- which(is.na(mat.res)) #this gives a scalar, as internally matrices are stored as vectors
    wrongvalue <- missingvalue-nrow(mat.res) #get the element where is the discrimination parameter has ended up
    mat.res[missingvalue] <- mat.res[wrongvalue]
    mat.res[wrongvalue] <- NA

    categories <- lapply(gpcm, names)
    categorynames <- categories[[which.max(sapply(categories, length))]]
    colnames(mat.res) <- categorynames
    return(mat.res)
  }
  mat.res
}

## this function doesn't work, ignore.
## newscrub <- function(x, isvalue=NULL, newvalue=NULL) {
##   stopifnot(isvalue !=NULL, newvalue !=NULL)
##   maxlength <- dim(x)[2]
##   for (i in 1:maxlength) {
##     k <- i
##     if(k>length(newvalue||isvalue)) {
##       k <- length(isvalue)
##     }
##     ifelse(newvalue[k]!=NA, x[!is.na(isvalue[k]),i] <- newvalue[k], isvalue)
##     x
##   }
##   return(x)
## }
RecodeMany <- function (data, vars, Recodings){
  varlist=list(vars)
dataret <- data
  for (i in 1:length(vars)) {
    dataret[,i] <- car::recode(data[,i], recodes=Recodings)
  }
return(dataret)
}
    ##   return(randitems)
## }
createSumScores <- function(data) {
data$physfun <- rowMeans(data[,grep("RANDQ[3456789]$|RANDQ[1][012]$",x=names(data))], na.rm=TRUE)
data$rolelim <- rowMeans(data[,grep("RANDQ[1][3456]$",x=names(data))], na.rm=TRUE)
data$rolelimem <- rowMeans(data[,grep("RANDQ[1][789]$",x=names(data))], na.rm=TRUE)
data$energyfat <- rowMeans(data[,grep("RANDQ[2][379]$|RANDQ31$",x=names(data))], na.rm=TRUE)
data$emwellbeing <- rowMeans(data[,grep("RANDQ[2][4568]$|RANDQ30$",x=names(data))], na.rm=TRUE)
data$socialfunctioning <- rowMeans(data[,grep("RANDQ20|RANDQ32",x=names(data))], na.rm=TRUE)
data$pain <- rowMeans(data[,grep("RANDQ[2][12]$",x=names(data))], na.rm=TRUE)
data$generalhealth <- rowMeans(data[,grep("RANDQ1$|RANDQ[3][3456]$",x=names(data))], na.rm=TRUE)
data$mindfulness <- rowMeans(data[, grep("MAAS", x=names(data))], na.rm=TRUE)
data$optimism <- rowMeans(data[,grep("LOTR", x=names(data))], na.rm=TRUE)
return(data)
}
TrainTestSets <- function (x, data) {
  testlist <- list()
  trainlist <- list()
  for (i in 1:length(x)) {
    split <- x[[i]]
    fold.train <- data[split,]
    fold.test <- data[-eval(split),]
    trainlist[[i]] <- fold.train
    testlist[[i]] <- fold.test
  }
  traintestlist <- c(trainlist, testlist)

  train.names <- paste(names(x), ".Train",sep="")
  test.names <- paste(names(x), ".Test",sep="")
  listnames <- c(train.names, test.names)
  names(traintestlist) <- listnames
  traintestlist
}
SeperateTestandTrain <- function(data, test=TRUE) {
  if(test) {
    indtest <- grep("Test$", names(data))
    res <- data[indtest]
  }
  else {
    indtrain <- grep("Train$", names(data))
    res <- data[indtrain]
  }
  res
}
Trainfolds <- function(data, Form, control, sizes, metric, updown) {
  cvresults <- list()
  for (i in 1:length(data)) {
    res <- train(form=Form, data=data, na.action="na.omit", size=sizes, metric=metric, maximise=updown, control=rfeControl)
    cvresults[[i]] <- res
  }
  names(cvresults) <- names(data)
  cvresults
}

Svdcv <- function(x, ...) {
  msep <- x$msep
  K <- nrow(msep)
  rank <- seq(from = 0, to = x$maxrank, by = 1)
    msep.mean <- apply(x$msep, 2, mean)
    msep.se <- apply(x$msep, 2, sd)/sqrt(K)
  res <- as.data.frame(cbind(rank, msep.mean, msep.se))
  names(res) <- c("Rank", "Prediction Error", "Prediction Error SE")
  resxtab <- xtable(res, ...)
}
getIrtPreds <- function (x) {
  res <- x$score.dat[,c("Obs", "Exp", "z1","se.z1")]
  res
}
compareIRTscores <- function (x, y) {
  scores.x <- x$z1
  scores.y <- y$z1
  cor.xy <- cor(scores.x, scores.y, method="pearson", use="pairwise.complete.obs")
  diff.xy <- (scores.x-scores.y)^2
  res <- list(cor=cor.xy, differences=diff.xy)
  res
}
splitSample <- function(x, split) {
  xlen <- nrow(x)
  indices <- sample(1:xlen, xlen, replace=FALSE)
  splitlen <- xlen/split
  splits <- cut(indices, split, labels=FALSE)
  samplist <- list()
  for(i in 1:max(split)) {
    samp<-x[splits==i,]
    assign(paste("samp", i, sep=""), value=samp, 1)
           samplist[[i]] <- get(paste("samp",i, sep=""))
    ## names(samplist[i]) <- paste("split", i, sep="")
  }
samplist
}
IRTcv <- function (data, model=c("grm", "gpcm"), constraint=c(TRUE, FALSE, "rasch", "1PL", "gpcm"), splits=10, ....) {
  if(is.dataframe(data) ||is.matrix(data))
    stop("this function needs matrix or dataframe input")
  splittedsamples <- splitSample(data, splits)
  for (i in 1:length(splittedsamples)) {
    testset <- splittedsamples[i]
    trainset <- splittedsamples[!i]
  }
}
IrtCV <- function(x) {

#get observed frequencies from display command in package ltm
obs <- descript(x)$perc
totscores <- descript(x)$items
totscores[totscores==0] <- NA
model <- grm(x)
model.scores <- factor.scores(model, resp.patterns=x)
abilities <- model.scores$score.dat["z1"]
pointsweights <- model$GH
cutpoints <- pointsweights[[1]]
weights <- pointsweights[[2]]
q <- seq(from=0, to=1, by=0.05) #create 21 points
quadnorm <- qnorm(q) # map 21 points to the normal quantiles
totscores2 <- rowSums(x, na.rm=TRUE)
totscores2[totscores2==0] <- NA
ab.scores <- as.matrix(cbind(totscores2, abilities))
res <- list(obsscores=obs, totscores=totscores2, abscores=ab.scores, model=model, scores=model.scores, abilities=abilities, weights=weights)
}
## getscores <- function(x) { #this function appears to be completely pointless, in fact it may also have been causing my problems with the function below.
##   reslist <- list()
##   x <- as.data.frame(x)
##   probmat <- matrix(NA, nrow=100, ncol=30)
##   uniquescores <- unique(x[,1])
##   unique.sorted <- sort(uniquescores)
##   for(i in seq_along(unique.sorted)) {
##     scoremat <- x[x$totscores2==unique.sorted[i],]
##     uniqueabs <- unique(scoremat[,2])
##     rep1 <- uniqueabs
##     reslist[[i]] <- rep1
##     names(reslist[[i]]) <- paste("score", unique.sorted[i], sep="")
##   }
##   reslist
## }
probcalc <- function(x, totscores) {
    res <- sapply(x, calcprob, totscores)
  }

calcprob <- function (x) {
  x2 <- x[,2]
  totscores <- x[,1]
  probcal <- list()
   for(i in seq_along(x2)) {
     if(is.na(x[[i]][1])) {
       next
     } else {
       y <- x[[i]]
       y <- sort(y)
       for(j in seq_along(y)) {
         if(is.na(y[1][j])) {
           next
           probcal[[i]] <- NA
         }
         else{
           p1 <- na.omit(length((y==y[j])))/ length(totscores)
           p2 <- length(y==y[j])/length(na.omit(totscores))
           p3 <- na.omit(length(y[j]))/length(na.omit(y))
           ## browser()
           p4 <- p1*p2
           p5 <- p4/p3
           probcal[[i]] <- p5
         }
       }

       probcal
     }
     probcal
   }
  probcal
}

#Estimate conditional distribution of test scores for each trait level p(Ability|score)= p(score)*p(ability)/p(score)
#p(abilities|totscores) (probably need to merge them into one dataframe for this).
#p(abilities|totscores)=P(ability)*P(totscores)/p(abilities)
#Bin participants on total scores
#Get scores for each participant (take from 0, to match typical IRT practice and the interpretation of scores as the number of thresholds successfully completed.
#This should be  distributed as a generalised multinomial for a polytomous model Use fitdistr from MASS to get best parameters of the distribution.
# Method 1: multiply the conditional probability by the weight associated with the quadrature point. (these are stored in the grm model as GH).  This provides an estimation of the expected proportion of participants having an observed test score.
#multiply the expected proportion by the sample size, the result is the expected frequency of of participants having a particular test score
#Method 2: if individual ability estimates are available for all participants, the marginal expected frequency for a given score is the sum of the conditional probabilities for this score across the N participants.
#This can be displayed graphically. In addition, a chi square test can be performed between the observed and expected frequencies, to give a measure of model mis-fit (with all the problems that the Chi-square test is heir too).
CondProbIrt <- function(x) {
  abilities <- x[,1]
  totscores <- x[,2]
  s.ord <- order(x$totscores2)
  x.ord <- x[s.ord,]
  x.ord2 <- na.omit(x.ord)
  scores.len <- with(x.ord,tapply(z1, totscores2, length))
  ## browser()
  unique.ab <- with(x.ord,xtabs(z1~totscores2))
  unique.scores <- as.numeric(names(scores.len))
  probmat <- as.data.frame(matrix(NA, ncol=20, nrow=100))
  for(i in seq_along(unique.scores)) {
    cur.score <- x.ord2[x.ord2$totscores2==unique.scores[i],]
    unique.ab <- with(cur.score,table(z1, totscores2))
    unique.sc <- with(x.ord2, table(totscores2))
    for(j in seq_along(unique.ab)) {
      p1 <- unique.ab[j]/length(unique(x.ord2$z1)) #ab prob
      p2 <- nrow(cur.score)/nrow(x.ord2) #score prob
      p3 <- p1*p2
      p4 <- p3/p2
      probmat[j,i] <- p4
    }
    probmat
  }
  scorenames <- paste("Score", unique.scores, sep="")
  names(probmat) <- scorenames
  probmat
}

##' .. content for \description{A function to calculate the IAT score (the D measure from Greenwald (2003)} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Calculate the IAT score of a dataset
##' @param data - a dataframe containing the stimuli names and response times, with one row per block
##' @param Code the participant identifier code
##' @param method either use mean or median to calculate scores
##' @param words a vector of strings giving the names of the stimuli
##' @return a dataframe containing the mean response times for each of the critical blocks (3 and 5) and the IAT score calculated by the specified method
##' @author Richard Morrisroe
calcIatScores <- function(data, Code, method=c("mean", "median"), words) {
  if(nrow(data)%%5!=0) {
    warning("not all participants have complete responses")

  partlen <- with(data, tapply(Block, Code,length))
    
  droppart <- partlen[partlen!=5]
  drop <- which(data$Code==names(droppart))
  data <- data[-(drop),]
  }
  if(method=="mean") {
    
    func <- method[1]
  }
  else {
    func <- method[2]
  }

  data2 <- data[,c(Code, words)]
  
  block3 <- data2[data$Block=="Block 3",]
  block5 <- data2[data$Block=="Block 5",]
  block1 <- data2[data$Block=="Block 1",]
  block2 <- data2[data$Block=="Block 2",]
  block4 <- data2[data$Block=="Block 4",]
  ## b3.corr <- block3$Correct
  ## b5.corr <- block5$Correct
  ## b1.corr <- block1$Correct
  ## b2.corr <- block2$Correct
  ## b4.corr <- block4$Correct
  ## block3 <- block3[,-(length(block3))]
  ## block5 <- block5[,-(length(block5))]
  stimblock3 <- block3[,words]
  stand.dev3 <- apply(stimblock3[,words], 1, sd, na.rm=TRUE)
  stimblock5 <- block5[,words]
  stand.dev5 <-  apply(stimblock5[,words], 1, sd, na.rm=TRUE)
  if(method=="mean") {
  b3score <- apply(stimblock3[,words], 1, mean, na.rm=TRUE)
  b5score <- apply(stimblock5[,words], 1,  mean, na.rm=TRUE)
}
  else {
    b3score <- apply(stimblock3[,words], 1, median, na.rm=TRUE)
  b5score <- apply(stimblock5[,words], 1,  median, na.rm=TRUE)
  }
  block3scores <- as.data.frame(cbind(b3score, stand.dev3))
  block5scores <- as.data.frame(cbind(b5score, stand.dev5))
  scores <-  cbind(block3[,Code],block3scores, block5scores)
  overallsd <-  (stand.dev3+stand.dev5)/2
  diff <-  b5score-b3score
iatscore <-  diff/overallsd
## browser()
res <- data.frame(scores=scores, IAT=iatscore, Block3=stimblock3, Block5=stimblock5 ## Block1Correct=b1.corr,Block2Correct=b2.corr,Block4Correct=b4.corr,Block3Correc
                  ## t=b3.corr, Block5Correct=b5.corr
                  )
}
fileImport <- function(directory, pattern) {
  files <- list.files(directory, pattern=pattern, full.names=TRUE)
  file.list <- lapply(files, read.table, header=FALSE)
  files2 <- gsub(".*-.*-([0-9][0-9][0-9][0-9][0-9]?).txt", "\\1", x=files)
  names(file.list) <- files2
  file.list
  ## file.df <- do.call(cbind, file.list)
}
listToDf <- function(data, ind) {
  dnames <- names(data)
  rows <- sapply(data, nrow)
  maxrows <- max(rows)
  cols <- length(data)
  res <- matrix(NA, nrow=maxrows, ncol=cols)
for (i in seq(from=1, to=length(data))) {
  temp <- data[[i]]
  res[1:nrow(temp),i] <- temp[,ind]
}
  if(ind==1) {
    colnames(res) <- paste("GSR", dnames, sep="")
p  }
  if(ind==2) {
    colnames(res) <- paste("ECG", dnames, sep="")
  }
  res
}
iatDiff <- function(x, y) {
  res <- matrix(NA, ncol=length(x), nrow=nrow(x))
  for (i in 1:length(x)) {
    print(i)
    ## browser()
    res[,i] <- mapply("-", x[,i], y[,i])
    res}
  stimnames <- names(x)
  stimnames2 <- paste(stimnames, ".Diff", sep="")
  names(res) <- stimnames2
  res}
repeatCV <- function(form, data, method=method, n, responsevariable, ...) {
  res <- vector(length=n, mode="list")
  Accuracy <- vector(length=n, mode="numeric")
  data2 <- na.omit(iatandexpfull)
  variable <- grep(responsevariable, x=names(data))
  for (i in 1:n) {
    print(i)
    trainind <- with(data2, createDataPartition(data[,variable], p=0.8, list=FALSE))
    trainset <- data2[trainind,]
    testset <- data2[-trainind,]
    train.res <- train(formula=form, data=trainset, ...)

    train.pred <- predict(train.res, testset)
    res[[i]] <- confusionMatrix(train.pred, testset[,responsevariable])
    Accuracy[i] <- res[[i]]$overall[1]
  }

  res2 <- list(res, Accuracy)
}


getIRTestimates <- function(fscores) {
  data <- fscores[["score.dat"]]
  abest <- data[,c("z1", "se.z1")]
  names(abest) <- c("AbilityEst", "StdError")
  return(abest)
}
testIRTModels <- function(oldmodel, newdata, gpcmconstraint=c("rasch", "1PL", "gpcm",), grmconstraint= c(TRUE, FALSE), ...) {
  if(class(oldmodel)=="gpcm") {
    constraint <- gpcmconstraint
  }
     else {
       constraint <- grmconstraint
     }

  comp.para <- length(unique(as.vector(coef(oldmodel))))
  predscores <- getIRTestimates(factor.scores(oldmodel, resp.patterns=newdata))
  if(class(oldmodel)=="gpcm") {
  newmodel <- gpcm(newdata, constraint=constraint)
}
  else {
    newmodel <- grm(newdata, constrained=constraint)
  }
  newscores <- getIRTestimates(factor.scores(newmodel, resp.patterns=newdata))
  diffscores <- mapply("-", predscores[,1], newscores[,1])
  rea <- sqrt(sum(diffscores^2))*log(comp.para)
  scorescor <- cor(predscores[,1], newscores[,1], ...)
  res <- data.frame(ErrorApproximation=rea, Correlation=scorescor)
  return(res)
}


penalisedRegression <- function(x, y,  testdata, newy, alpha, nfolds=10,type=c("coefficients", "response"), family="gaussian") {
  x.mat <- as.matrix(x)
  testdata.mat <- as.matrix(testdata)
  ## browser()
  cvres <- cv.glmnet(x=x.mat, y=y, nfolds=nfolds, family=family)
  mod <- glmnet(x=x.mat, y=y, alpha=alpha, family=family)
  pred.coef <- predict(mod, testdata.mat, s=cvres$lambda.min, type=type)
  if(type=="response") {
  pred <- data.frame(pred=as.vector(pred.coef), obs=newy)
  return(pred)
}
  else{
    return(pred.coef)
  }
  }
tuneLoess <- function(formula, data, newdata, tuneLength, ...) {
  formula <- as.formula(formula)
  seq <- seq(0, 1, by=tuneLength)
  part <- list(length=length(seq))
  part.index <- createDataPartition(formula[2], p=0.8, times=length(seq))
  for (i in seq_along(part.index)) {
    part[[i]] <- data[part.index,]
  }
  fitlist <- vector(mode="list", length=length(seq))
  for (j in seq_along(tuneLength)) {
    fitlist[[j]] <- loess(formula, data=part[[j]], span=seq[i])
          }
    fitlist
}
lazyload <- function (files, names, cols) {
  filetype <- paste(names, "-", sep="")
  outfilenames <- gsub("Richi[e]?-",filetype, x=files)
  outfilenames2 <- gsub(".*/Richieoutput/", "", x=outfilenames)
  for (i in 1:length(files)) {
    temp <- read.table(files[i])
    ## browser()
    gsr <- temp[,cols]
    write.table(gsr, file=outfilenames2[i])
  }
}
    
rmsea <- function(data) {
    erro <- with(data, pred-obs)
    err.sq <- erro^2
    root.err <- sqrt(mean(err.sq))
    return(root.err)
}
        

lazylength <- function(files) {
    tp <- gsub(".*/", "", x=files)
    tp.split <- strsplit(as.character(tp), "-")
    pp <- lapply(tp.split, "[", 3)
    pp <- gsub(".txt", "", x=pp)
    ## browser()
    lengthmat <- matrix(NA, 114, ncol=2)
    for (i in 1:length(files)) {
        temp <- read.table(files[i])
        len <- dim(temp)[1]
        lengthmat[i,1] <- pp[i]
        lengthmat[i,2] <- len
        rm(temp); gc()
    }
    lengthmat
}



getPPNo <- function(files) {
    tp <- gsub(".*/", "", x=files)
    tp.split <- strsplit(as.character(tp), "-")
    pp <- lapply(tp.split, "[", 3)
    pp <- gsub(".txt", "", x=pp)
    pp
}
lazylength <- function(files) {
    pp <- getPPNo(files)
    ## browser()
    lengthmat <- matrix(NA, 114, ncol=2)
    for (i in 1:length(files)) {
        temp <- read.table(files[i])
        len <- dim(temp)[1]
        lengthmat[i,1] <- pp[i]
        lengthmat[i,2] <- len
        rm(temp); gc()
    }
    lengthmat
}
lazymean <- function( path, pattern, ...) {
    lsfiles <- list.files(path, pattern, ...)
    pp <- getPPNo(lsfiles)
    meanmat <- matrix(NA, length(lsfiles), ncol=2)
    for(i in 1:length(lsfiles)) {
        temp <- read.table(lsfiles[i])
        mu <- mean(temp[,1])
        meanmat[i,] <- c(pp[i], mu)
}
    meanmat
}

lazydownsample <- function(path, pattern, aggregate=1000, FUN=mean, ...) {
  stopifnot(is.numeric(aggregate))
    lsfiles <- list.files(path, pattern, full.names=TRUE)
    pp <- getPPNo(lsfiles)
    mymat <- matrix(NA, 3200, ncol=length(lsfiles))
    for(i in 1:length(lsfiles)) {

        temp <- read.table(lsfiles[i])
        dim <- dim(temp)[1]
        dimsec <- ceiling(dim/aggregate)
        myrep <- sort(rep(1:dimsec, length.out=dim))
                temp[,"myrep"] <- as.factor(myrep)
        ds <- as.data.frame(with(temp, tapply(x, myrep, FUN, ...)))
        print(i)
        dimds <- dim(ds)[1]
        mymat[1:dimds,i] <- ds[,1]
    }
    colnames(mymat) <- pp
    mymat
}
        
difffunc <- function(data) {
  max <- which.max(data)
  min <- which.min(data)
  time <- abs(max-min)
  return(time)
}
interpolate.pain <- function(pain, padding) {
    max.padding <- with(padding, max(FirstPainRating, na.rm=TRUE))
    pain.sec <- 45*60 #hack, as the experiment was 45 mins max following pain induction
    max.len <- pain.sec+max.padding+1 #for participant column
    row.nums <- with(pain, length(unique(Participant)))
    res.mat <- matrix(NA, nrow=row.nums, ncol=max.len)
    pain.merge <- merge(pain, padding, by.y="PPNo.", by.x="Participant")
    partno <- with(padding, PPNo.)
    part.pain.sec <- apply(pain[,with(pain,
                                      grep("^X", x=names(pain)))],
                           c(1,2), function (x) rep(x, times=60))
    
    for(i in seq_along(partno)) {
        print(partno[i])
        nowpart <- partno[i]
        ## browser()
        len.part <- padding[with(padding, PPNo.==nowpart),]
        start.time <- len.part[,3]
        if(is.na(start.time)) {
            next}
        mypadding <- vector(mode="numeric", length=start.time)
        full.dat <- c(mypadding, part.pain.sec[,i,])
        ## browser()
        res.mat[i,1:length(full.dat)+1] <- full.dat
        res.mat
        ## browser()
    }
        res.mat
    
    }

interpolate2 <- function(painscores, painmetadata) {
    pain.ratings.min <- with(painmetadata, floor((SqueezStop+60)/60))
    pain.ratings.min <- as.data.frame(pain.ratings.min)
    pain.ratings.min[,"Participant"] <- painmetadata$PPNo.
    names(pain.ratings.min)[1] <- "padding"
    resmat <- matrix(0, nrow=nrow(pain.ratings.min), ncol=45+with(pain.ratings.min, max(padding, na.rm=TRUE)))
    partno <- pain.ratings.min$Participant
    painscores.real <- grep("^X", x=names(painscores))
    for (i in seq_along(partno)) {
        print(i)
        partpad <- pain.ratings.min[with(pain.ratings.min,Participant==partno[i]),1]
        if(is.na(partpad)) {
            next}
        
        painratings <- as.numeric(painscores[with(painscores, Participant==partno[i]),painscores.real])
        if(i==27) {
        ## browser()
        }
        padding <- rep(0, times=partpad)
        padpluspain <- c(padding, painratings)
        
        resmat[i,1:length(padpluspain)] <- padpluspain
        resmat}
    resmat
}
apademotables <- function(data, FUN=mean, xtable=FALSE, ...) {
    fun <- match.call(FUN)
    ## browser()
    data.m <- melt(data)
    data.tab <- ddply(data.m, .(variable), summarise, Mean=mean(value, na.rm=TRUE), SD=sd(value, na.rm=TRUE), Median=median(value, na.rm=TRUE),  Min=min(value, na.rm=TRUE), Max=max(value, na.rm=TRUE))
    names(data.tab)[1] <- ""
    if(xtable==TRUE) {
        data.tab <- xtable(data.tab)
    }
    return(data.tab)
}
FactorAverage <- function (sols=list(), mynames=NULL, FUN=mean, correlations=FALSE, ....) {

    sols.coeff.list <- list()
    
    for(i in 1:length(sols)) {
        if(correlations) {
            coeff <- as.data.frame(FactorCor(sols[[i]], xtable=FALSE))
        }
         else {                          
             coeff <- as.data.frame(FactorCoeff(sols[[i]]))
    }
        ## browser()
        coeff.ord <- coeff[,mynames]
        sols.coeff.list[[i]] <- coeff.ord
        
    }
    sols.coeff.list
    sols.list <- lapply(sols.coeff.list, as.matrix)
    resmat <- apply(simplify2array(sols.list), c(1,2), FUN)
    resdims <- dim(resmat)[2]-1
    resmat[] <- sapply(resmat, round, 2)
    if(!correlations) {
    resmat[,1:resdims] <- sapply(resmat[,1:resdims], function (x) ifelse(x>=0.3, paste0("\\textbf{",  x, "}"), x))
}
    ## resmat <- Reduce(`+`, sols.coeff.list)/length(sols.coeff.list)
    ## dimmat <- dim(sols.coeff.list[[1]])
    ## resmat <- matrix(0, nrow=dimmat[1], ncol=dimmat[2])
    ## colnames(resmat) <- names
    ## for (i in 1:length(sols.coeff.list)) {
    ##     ## if(i==3)        browser()
    
    ##     resmat <- (resmat+sols.coeff.list[[i]])/2
    ## }
    return(resmat)
}
FactorNames <- function(fac, names=NULL) {
    if(is.null(names)) {
        stop("Calling a function based on factor names with no names seems like a bad idea, don't you think?")
    }
    
    colnames(fac$loadings) <- names
    return(fac)
}
getMxFitFunctions <- function(mx, label=NULL) {
    summ <- summary(mx)
    bic <- summ$BIC.Mx
    aic <- summ$AIC.Mx
    obs <- summ$numObs
    param <- summ$estimatedParameters

    res <- data.frame( bic, aic, obs, param)
    if(!is.null(label)) {
        rownames(res) <- label
    }
    return(res)
}
irtAverage <- function(sols=list()) {
    coef <- lapply(sols, coef)
    res <- Reduce(`+`, x=coef)/length(coef)
    return(res)
}
            
smoothAIC <- function(model) {
    if(class(model) %in% c("MxRAMModel", "MxModel")) {
        res <- summary(model)$AIC.Mx
    }
    else {
        res <- AIC(model)
    }
    return(res)
}
smoothedAIC <- function (models) {
    information <- lapply(models, smoothAIC)
    exp.info <- lapply(information, function(x) exp(-0.5*x))
    info <- Reduce(`+`, exp.info)
    weights <- vector(mode="numeric", length=length(information))
    for (i in 1:length(information)) {
        weights[i] <- exp(-0.5*information[[i]])/info
        ## browser()
    }
    return(weights)
}
        
irtAverageFactorScores <- function (scores=list) {
    abilities <- sapply(scores, `[`, 1)
    ab.average <- Reduce(`+`, abilities)/length(abilities)
    names(ab.average) <- "AbilityEst"
    return(ab.average)
}
provide <- function(name, ...) {
    if(!(require(name))) {
        install.packages(name, ...)
    }
    }
coefirt <- function(grm) {
    if(class(grm)=="grm" || class(grm)=="gpcm") {
    dat <- coef(grm)
}
    else {
        dat <- grm
    }
    
    dims <- dim(dat)
    betas <- dims[2]-1
    #just need to add an identity sanitize test function to get the alphas and betas as markup
    beta.names <- paste("$", "\\beta",  "^", 1:betas, "$", sep="")
    alpha.name <- "$\\alpha$"
    ## beta.names <- paste("beta", 1:betas, sep="")
    ## alpha.name <- "alpha"
    allnames <- c(beta.names, alpha.name)
    ## browser()
    colnames(dat) <- allnames
    ## rownames(dat)[1] <- "Item"
    ## browser()
    dat
}
apareg <- function(model, logistic=FALSE) {
    mod.sum <- summary(model)
    mod.coef <- coef(mod.sum)
    colnames(mod.coef) <- c( "B", "SE(B)", "t", "Sig.(p)")
    if(logistic) {
        colnames(mod.coef) <- c("B", "SE(B)", "z", "Sig.(p)")
    }
    standardised <- lm.beta(model)
    standardised2 <- c(NA, standardised)
    ## browser()
    mod.coef <- as.data.frame(mod.coef)
    mod.coef["$\\beta$"] <- standardised2

    mod.coef <- mod.coef[,c(1, 2, 5, 3, 4)]
    mod.coef
}
