
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

## Part 2 -------------

# The method used in part 1 solve() function is not going to work - the loop will take a VERY long time!

solve2 <- function(a.x, a.y, b.x, b.y, prize.x, prize.y, offset=0) {
  
  # It's the intersection of two slopes!
  # This equation was worked out on paper, sorry..
  
  a <- (offset+prize.x)/b.x - (offset+prize.y)/b.y
  a <- round(a / (a.x/b.x - a.y/b.y),0)
  b <- round(((prize.x + offset) -a*a.x) / b.x,0)

  # Look for rounding errors in the big equation
  if (a * a.x + b * b.x != prize.x + offset) return(0)
  if (a * a.y + b * b.y != prize.y + offset) return(0)
  return(a*3+b)
}


tot <- 0
for (i in 1:nrow(df)) {
  tot <- tot + solve2(df[i,1],df[i,2],df[i,3],df[i,4],df[i,5],df[i,6], offset=10000000000000)
  print(".")
}
tot

i = 4
solve2(df[i,1],df[i,2],df[i,3],df[i,4],df[i,5],df[i,6], offset=10000000000000)



