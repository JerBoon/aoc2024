
## Parsing the file

dat <- readLines("data/13.test.dat")
dat <- readLines("data/13.dat")

# We have a format with groups of 6 numbers repeating, so let's just smudge
# all the input together and split out the #s

nums <- strsplit(paste(dat,collapse=" "),split="[^0-9]+")[[1]]
nums <- as.numeric(nums[2:length(nums)] )
df <- as.data.frame(matrix(nums,ncol=6,byrow=T))

## Part 1

solve <- function(a.x, a.y, b.x, b.y, prize.x, prize.y) {

  # b is cheaper, so start with the biggest possible b and work backwards
  for(b in min(prize.y%/%b.y, prize.x%/%b.x):0) {
    if ((prize.x - b*b.x)%%a.x == 0 &
        (prize.y - b*b.y)%%a.y == 0 &
        (prize.x - b*b.x)%/%a.x == (prize.y - b*b.y)%/%a.y) {
      a <- (prize.x - b*b.x)%/%a.x
      return(a*3+b*1) # number of tokens spent
    }
  } 
  return(0)
}

tot <- 0
for (i in 1:nrow(df)) {
  tot <- tot + solve(df[i,1],df[i,2],df[i,3],df[i,4],df[i,5],df[i,6])
}
tot

