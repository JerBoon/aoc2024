
dat <- readLines("data/4.test.dat")
dat <- readLines("data/4.dat")

cols <- nchar(dat[1])
rows <- length(dat)
mat <- matrix(unlist(strsplit(dat,"")), nrow=rows, ncol=cols, byrow=T)


find.word <- function(word, x, y, xd, yd) {
  if (((x + xd*(nchar(word)-1)) %in% 1:cols) &
      ((y + yd*(nchar(word)-1)) %in% 1:rows)) {
    for(i in 1:nchar(word))
      if (mat[y+yd*(i-1),x+xd*(i-1)] != substr(word,i,i))
        return(0)
    return(1)
  }
  return(0)
}


## part 1 

tot <- 0
for (x in 1:cols) {
  for (y in 1:rows) {
    tot <- tot +
      find.word("XMAS",x,y, 1, 0) +
      find.word("XMAS",x,y, 1, 1) +
      find.word("XMAS",x,y, 0, 1) +
      find.word("XMAS",x,y,-1, 1) +
      find.word("XMAS",x,y,-1, 0) +
      find.word("XMAS",x,y,-1,-1) +
      find.word("XMAS",x,y, 0,-1) +
      find.word("XMAS",x,y, 1,-1)
  }
}
tot


## part 2

# mod to also check starts are in bunds
find.word <- function(word, x, y, xd, yd) {
  if ((x %in% 1:rows) &
      (y %in% 1:cols) &
      ((x + xd*(nchar(word)-1)) %in% 1:cols) &
      ((y + yd*(nchar(word)-1)) %in% 1:rows)) {
    for(i in 1:nchar(word))
      if (mat[y+yd*(i-1),x+xd*(i-1)] != substr(word,i,i))
        return(0)
    return(1)
  }
  return(0)
}

tot <- 0
for (x in 1:cols) {
  for (y in 1:rows) {
    tot <- tot +
      find.word("MAS",x,y, 1, 1) * find.word("MAS",x+2,y,-1, 1)  +
      find.word("MAS",x,y, 1, 1) * find.word("MAS",x,y+2, 1,-1)  +
      find.word("MAS",x,y,-1,-1) * find.word("MAS",x-2,y, 1,-1)  +
      find.word("MAS",x,y,-1,-1) * find.word("MAS",x,y-2,-1, 1)  
  }
}
tot

