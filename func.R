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
  ## if(class(grm)=="gpcm") {
  ##  x <- coef2mat(grm)
  ## }
  ## else {
  x <- coef(grm)
## }
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
    dimcols <- len[[2]]
    dimrows <- length(names(gpcm))
    mat.res <- matrix(NA, nrow=dimrows, ncol=(dimcols+1))
    for (i in seq_along(gpcm)) {
      column <- lapply(gpcm, "[", i)
      column <- as.matrix(unlist(column))
      mat.res[,i] <- column
      mat.res
    }
    rownames(mat.res) <- names(gpcm)
    mat.res[1,5] <- mat.res[1,4]
    mat.res[1,4] <- NA
    categories <- lapply(gpcm, names)
    categories <- categories[[2]]
    categories[6] <- "NA"
    colnames(mat.res) <- categories
    return(mat.res)
  }
  mat.res
}
