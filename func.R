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
myscrub <-
  function (x, where, min, max, isvalue, newvalue)
  
{
  if (missing(min)) 
    min <- -Inf
  if (missing(max)) 
    max <- Inf
  if (missing(isvalue)) 
    isvalue <- Inf
  if (missing(where)) 
    where <- 1:dim(x)[2]
  maxlength <- max(length(isvalue), length(min), length(max), 
                   length(where))
  if (missing(newvalue)) 
    newvalue <- rep(NA, maxlength)
  if (length(min) == 1) 
    min <- rep(min, dim(x)[2])
  if (length(max) == 1) 
    max <- rep(max, dim(x)[2])
  if (length(isvalue) == 1) 
    isvalue <- rep(isvalue, maxlength)
  if (length(newvalue) == 1) 
    newvalue <- rep(newvalue, maxlength)
  x <- as.matrix(x)
  for (k in 1:maxlength) {
    i <- where[k]
    x[(!is.na(x[, i]) & (x[, i] < min[k])), i] <- newvalue[k]
    x[(!is.na(x[, i]) & (x[, i] > max[k])), i] <- newvalue[k]
    x[(!is.na(x[, i]) & (x[, i] == isvalue[k])), i] <- newvalue[k]
  }
  x <- as.data.frame(x)
  return(x)
}
newscrub <- function(x, isvalue=NULL, newvalue=NULL) {
  stopifnot(isvalue !=NULL, newvalue !=NULL)
  maxlength <- dim(x)[2]
  for (i in 1:maxlength) {
    k <- i
    if(k>length(newvalue||isvalue)) {
      k <- length(isvalue)
    }
    ifelse(newvalue[k]!=NA, x[!is.na(isvalue[k]),i] <- newvalue[k], isvalue)
    x
  }
  return(x)
}
