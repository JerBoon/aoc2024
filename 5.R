
dat <- readLines("data/5.test.dat")
dat <- readLines("data/5.dat")

rules <- dat[1:(which(dat == "")-1)]
prints <- dat[(which(dat == "")+1):length(dat)]


## part 1

tot <- 0
for (i in 1:length(prints)) {
  p <- strsplit(prints[i],",")[[1]]
  
  ok <- T
  for(j in 1:(length(p)-1))
    for (k in (j+1):length(p))
      if(paste(p[k],"|",p[j],sep="") %in% rules)
        ok <- F
  
  if (ok) {
    print(i)
    tot <- tot + as.numeric(p[(length(p)+1)/2])
  }
    
}
tot



## part 1

sorted <- function(p) {

  ok <- T
  for(j in 1:(length(p)-1))
    for (k in (j+1):length(p))
      if(paste(p[k],"|",p[j],sep="") %in% rules)
        ok <- F
  
  if (ok) {
    return(as.numeric(p[(length(p)+1)/2]))
  }
  return (0)  
}

tot <- 0
for (i in 1:length(prints)) {
  p <- strsplit(prints[i],",")[[1]]

  s <- sorted(p)
  if (s == 0) {
    o <- rep(0,length(p))
    for(j in 1:(length(p)-1))
      for (k in (j+1):length(p))
        if (paste(p[k],"|",p[j],sep="") %in% rules)
          o[k] <- o[k]+1
        else
          o[j] <- o[j]+1
    tot <- tot + as.numeric(p[(o == mean(o))])
  }
}
tot
