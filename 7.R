

dat <- readLines("data/7.test.dat")
dat <- readLines("data/7.dat")



## part 1 -----------

#return 0 if no solution, else the lhs vale
solve <- function(str) {
  
  nums <- as.numeric(strsplit(str, "[: ]+")[[1]])
  
  rec <- function(soln, tot, inputs) {
    
    #print(c("rec",soln,tot,inputs))
    for (op in c("+","*")) {
      if (op == "+")
        new_tot <- tot + inputs[1]
      else if (op == "*")
        new_tot <- tot * inputs[1]
      
      if (length(inputs) == 1) {
        if(new_tot == soln) return (soln)
      } else {
        if(rec(soln,new_tot,inputs[2:length(inputs)]) > 0) return(soln)
      }
    }
    return (0) # only gets here if no solution

  }
  
  return (rec(nums[1], nums[2], nums[3:length(nums)]))
}

#for (i in 1:length(dat))   print(solve(dat[i]))

tot <- 0
for (i in 1:length(dat))   tot <- tot + solve(dat[i])
tot


## part 2 -----------

#return 0 if no solution, else the lhs vale
solve2 <- function(str) {
  
  nums <- as.numeric(strsplit(str, "[: ]+")[[1]])
  
  rec <- function(soln, tot, inputs) {
    
    #print(c("rec",soln,tot,inputs))
    for (op in c("+","*","||")) {
      if (op == "+")
        new_tot <- tot + inputs[1]
      else if (op == "*")
        new_tot <- tot * inputs[1]
      else if (op == "||")
        new_tot <- as.numeric(paste(tot, inputs[1],sep=""))
      
      if (length(inputs) == 1) {
        if(new_tot == soln) return (soln)
      } else {
        if(rec(soln,new_tot,inputs[2:length(inputs)]) > 0) return(soln)
      }
    }
    return (0) # only gets here if no solution
    
  }
  
  return (rec(nums[1], nums[2], nums[3:length(nums)]))
}

#for (i in 1:length(dat))   print(solve(dat[i]))

options(scipen = 20)  #both for display AND the paste() to work!

# 3^n is hella lot slower than 2^n !!!
tot <- 0
for (i in 1:length(dat))  {
  tot <- tot + solve2(dat[i])
  print(c(i,tot))
}
tot

