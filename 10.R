
dat <- readLines("data/10.test.dat")
dat <- readLines("data/10.dat")

cols <- nchar(dat[1])
rows <- length(dat)
mat <- matrix(unlist(strsplit(dat,"")), nrow=rows, ncol=cols, byrow=T)


# Part 1

score <- function(x,y, method=1) {
  
  climb <- function(trailhead,x,y,height) {
    
    #print(c(trailhead,x,y,height))
    if(height == 9) {
      res <<- c(res, paste(trailhead," to ",y,",",x,sep=""))
      #print(c(trailhead,paste(y,x,sep=",")))
    } else {
      if (x > 1 && mat[y,x-1] == (height+1))
        climb(trailhead,x-1,y,height+1)
      if (x < cols && mat[y,x+1] == (height+1))
        climb(trailhead,x+1,y,height+1)
      if (y > 1 && mat[y-1,x] == (height+1))
        climb(trailhead,x,y-1,height+1)
      if (y < rows && mat[y+1,x] == (height+1))
        climb(trailhead,x,y+1,height+1)
    }
  }
  
  res <- character() 
  climb(paste(y,x,sep=","), x, y, 0)

  if (method==1)
    return(length(unique(res)))
  else
    return(length(res))
}

mat
score(3,1)

tot <- 0
for (y in 1:rows)
  for (x in 1:cols)
    if(mat[y,x] == "0")
      tot <- tot + score(x,y)
tot


# part 2 -------------

# Easy tweak! Added a method and switch to the return statement of score()

tot <- 0
for (y in 1:rows)
  for (x in 1:cols)
    if(mat[y,x] == "0")
      tot <- tot + score(x,y,method=2)
tot
