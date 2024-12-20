
dat <- readLines("data/2.test.dat")
dat <- readLines("data/2.dat")

dat

## part 1

safe <- 0
for (i in 1:length(dat)) {
  line <- as.numeric(strsplit(dat[i],split=" ")[[1]])
  if (all((line[1:(length(line)-1)] - line[2:length(line)]) %in% c(1,2,3)) |
      all((line[1:(length(line)-1)] - line[2:length(line)]) %in% c(-1,-2,-3)))
    safe <- safe + 1
}
safe


## part 2

safe <- 0
for (i in 1:length(dat)) {
  line <- as.numeric(strsplit(dat[i],split=" ")[[1]])
  safe.line <- 0
  
  for (j in 1:length(line)) {
    lineX <- line[-j]
    if (all((lineX[1:(length(lineX)-1)] - lineX[2:length(lineX)]) %in% c(1,2,3)) |
        all((lineX[1:(length(lineX)-1)] - lineX[2:length(lineX)]) %in% c(-1,-2,-3)))
      safe.line <- 1
  }
  safe <- safe + safe.line
}
safe

