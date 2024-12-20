

stones <- c(0,1,10,99,999)   # sample config


## part 1

blink <- function(stones) {
  
  i <- 1
  while (i <= length(stones)) {
    if (stones[i] == 0)
      stones[i] <- 1
    else if (nchar(as.character(stones[i]))%%2 == 0) {
      stones <- c({if(i>1) stones[1:(i-1)] else numeric()},
                  as.numeric(substr(as.character(stones[i]),1,nchar(as.character(stones[i]))/2)),
                             as.numeric(substr(as.character(stones[i]),nchar(as.character(stones[i]))/2+1, nchar(as.character(stones[i])))),
                  {if(i<length(stones)) stones[(i+1):length(stones)] else numeric()})
      i <- i+1
    } else 
      stones[i] <- stones[i]*2024
    
    i <- i + 1
  }
  
  return(stones)
}


stones <- c(0,0)
(stones <- blink(stones))

# example
stones <- c(125,17)
for (i in 1:6) {
  stones <- blink(stones)
  print(stones)
}

# real problem
stones <- c(6571,0,5851763,526746,23,69822,9,989)
for (i in 1:25) 
  stones <- blink(stones)
print(length(stones))


## Hmmm. Brute force seems quite slow. Seems we've reached the phase of advent where these start to get more tricksy
## and requiring more lateral thinking. Which to be fair is the fun part...
## I suspect part 2 will really start to do that!!

## Haha! yes ... 75 times. Erm.....


## part 2 -------------------------------------------------------------

# Ok, there's a certain amount of reusability here...
# After 25 iterations we have 203953 stones, but actually only 327 distinct vales!
# The stones evolve indepentenly, so we know that, e.g. the x11434 stones with a zero on them will
# generate the same patterns from here on, so surely should only need to work thsat out once!

stones <- c(6571,0,5851763,526746,23,69822,9,989)
stones<- 2
for (i in 1:5) {
  stones <- blink(stones)
  print(stones)
}

# we can see that after 5 or 6 iterations we already have lots of repetition
# So let's reimagine the simple list/vector of stones as a frequency table - 
# e.g. we have x6 stones with 2 them, and so on...

df <- data.frame(stone = c(6571,0,5851763,526746,23,69822,9,989), freq = 1)

blink2 <- function(df) {
  
  options(scipen=20)
  
  for (i in 1:nrow(df)) {
    if (df$stone[i] == 0)
      df$stone[i] <- 1
    else if (nchar(as.character(df$stone[i]))%%2 == 0) {
      df[nrow(df)+1, "stone"] <- as.numeric(substr(as.character(df$stone[i]),nchar(as.character(df$stone[i]))/2+1, nchar(as.character(df$stone[i]))))
      df$stone[i] <- as.numeric(substr(as.character(df$stone[i]),1,nchar(as.character(df$stone[i]))/2))
      df[nrow(df), "freq"] <- df$freq[i]
    } else 
      df$stone[i] <- df$stone[i]*2024
    
  }
  
  # then reorder!
  df2 <- data.frame(stone = sort(unique(df$stone)), freq=NA)
  for (i in 1:nrow(df2))
    df2$freq[i] <- sum(df$freq[df$stone == df2$stone[i]])
  
  return(df2)
}
df
(df <- blink2(df))

# The original problem = MUCH faster
df <- data.frame(stone = c(6571,0,5851763,526746,23,69822,9,989), freq = 1)
for (i in 1:25) 
  df <- blink2(df)
print(sum(df$freq))

# The new problem ...
df <- data.frame(stone = c(6571,0,5851763,526746,23,69822,9,989), freq = 1)
for (i in 1:75) {
  print(c(i, nrow(df))) # just to visualise how it performs
  df <- blink2(df)
  
}
print(sum(df$freq))


# Sweet! MUCH quicker - the data frame grows to about 3000 rows, then settles into very slow growth pattern
# Tempted to see how far we could take this (though we'll hit floating point precision issues soon though!)
# There are by now 12 trillion stones with a zero on!!!
