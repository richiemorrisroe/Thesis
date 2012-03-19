FactorXtab <-  function (x, ...) {
  dots <- (...)
x.load<-x$loadings 
x.comm<-x$communality
x.names <- colnames(x.load)
len <- length(colnames(x.load))
x.names[len+1] <- "Communalities"
x.comm.load<-cbind(x.load, x.comm)
x.mat.df<-as.matrix.data.frame(x.comm.load)
colnames(x.mat.df) <- x.names
#colnames(x.mat.df)[length(x.mat.df)] <- "Communalities"
fact.xtab <- xtable(x.mat.df, ...)
fact.xtab
}
FactorCor <- function (x, ...) {
  res <- x$score.cor
  #allnames <- attr(x$loadings, "dimnames")
  factnames <- colnames(x$loadings)
  res <- as.data.frame(res)
  #names(res) <- factnames
  res.x <- xtable(res, ...)
}
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
Extracth2u2 <- function (x) {
  x.comm <- x$communality
  x.uniq <- x$uniquenesses
  x.ratio <- x.comm/x.uniq
  x.h2u2 <- as.data.frame(cbind(x.comm, x.uniq, x.ratio))
  x.h2u2
 }
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
FitIndices <- function (x) {
  tli <- x$TLI
  bic <- x$BIC
  rmsea <- x$RMSEA
  rmsnames <- attr(x$RMSEA, "names")
  res <- as.data.frame(cbind(tli, bic, rmsea[1], rmsea[2], rmsea[3]))
  print(length(res))
  colnames(res) <- c(paste(substitute(x),"TLI", sep=""),paste(substitute(x),"BIC", sep=""),
                     paste(substitute(x),"RMSEA", sep=""),
                     paste(substitute(x),"-90CInt", sep=""),
                     paste(substitute(x),"+90CInt", sep=""))
  res
}
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
getLoadings <- function (mfa) {
  ind <- lapply(mfa, ExtractLoadings)
  ind
}
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
displayRot <- function (mfa, method=NULL, rotreq=NULL) {
  rotationreq <- rotreq
  meth <- method
  resind <- grep(rotationreq, x=names(mfa))
  res <- mfa[[resind]]
}
  
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
  plot1 <- ggplot(x.tm, aes(x=ability, y=item, shape=as.factor(threshold)), ...)
  plot2 <- plot1+layer(geom="point")
  plot2
  }
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
    categorynames <- categories[[length=ncol(mat.res)]]
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
  
