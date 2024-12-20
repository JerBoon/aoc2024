

map <- "2333133121414131402"
map <- readLines ("data/9.dat")


## part 1


# construct fs
fs1 <- numeric(0)
for (i in 1:nchar(map)) {
  fs1 <- c(fs1, rep(ifelse(i%%2, round(i/2-0.1), NA), as.integer(substr(map,i,i))))
}
print(fs1)


# defragment
fs2 <- fs1
for (i in length(fs2):sum(!is.na(fs2))) {
  if (!is.na(fs2[i])) {
    c <- fs2[i]
    fs2[i] <- NA
    fs2[min(which(is.na(fs2)))] <- c
    #print(paste(fs2,collapse=""))
  }
}
print(fs2)

# checksum
sum(fs2 * 0:(length(fs2)-1), na.rm=TRUE)
