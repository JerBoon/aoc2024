

dat <- read.table("data/1.test.dat", header=F, col.names=c("a","b"))
dat <- read.table("data/1.dat", header=F, col.names=c("a","b"))


## Part 1

sum(abs(sort(dat$a) - sort(dat$b)))


## Part 2

tot <- 0
for(i in 1:nrow(dat)) {
  
  tot <- tot + dat$a[i]*(sum(dat$a[i] == dat$b))
}
tot
