biodata <- ls(pattern="phys")
biologicaldata <- matrix(NA, nrow=1100000, ncol=110)
for (i in seq(from=1, to=length(biodata))) {
  temp <- get(biodata[i])
  print(i)
  biologicaldata[1:nrow(temp),i] <- temp[,1]
}





biologicaldata2 <- matrix(NA, nrow=200000, ncol=220)
biocollate <- function (x) {
  for (i in seq_along(x)) {
    temp <- get(x)[i]
    biologicaldata[1:length(x),c(i, i+1)] <- temp[,c(1,2)]
  }
}
    
