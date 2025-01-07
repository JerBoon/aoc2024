


## It's a trap!!
## There are only 8 instructions in the program, so why not just hard wire the code....?
## Gonna do that for part 1, and then see what shenanegans part 2 have in store to
## render this method obsolete....


## Part 1 -----------


run <- function(a,b,c,prog, part2 = FALSE) {
  ip <- 1
  #ps <- c(adv,bxl,bst,NA,NA,out,NA,NA)
  output <- numeric()

  combo <- function() {
    op <- prog[ip + 1]
    if (op == 0) return (0)
    if (op == 1) return (1)
    if (op == 2) return (2)
    if (op == 3) return (3)
    if (op == 4) return (a)
    if (op == 5) return (b)
    if (op == 6) return (c)
  }
  
  while(ip < length(prog)) {
    #print(paste(c("ip=",ip," exec",prog[ip+0:1]),collapse=" "))
    
    if (prog[ip] == 0) {  ## adv ##
      a <- floor(a / (2^combo()))
    } else if (prog[ip] == 1) {  ## bxl ##
      b <- bitwXor(b,prog[ip+1])
    } else if (prog[ip] == 2) {  ## bst ##
      b <- combo() %% 8
    } else if (prog[ip] == 3) {  ## jnz ##
      if (a != 0) { ip <- prog[ip+1]-1 }
    } else if (prog[ip] == 4) {  ## bxc ##
      b <- bitwXor(b,c)
    } else if (prog[ip] == 5) {  ## out ##
      output <- c(output,combo()%%8)
      if (part2) {
        # if part2 = TRUE, returns T/F rather than output vector
        # And in that case, can output FALSE as soon as the result *begins* to deviate = more efficent!
        if(!identical(output, c(prog,-99)[1:length(output)]))
          return(FALSE)
      }
    } else if (prog[ip] == 7) {  ## adv ##
        c <- floor(a / (2^combo()))
    } else{
      message(paste0("invalid op: ip=", ip))
      return()
    }
    ip <- ip + 2
    #Sys.sleep(0.1)
  }
  #print("complete")
  if (part2) 
    return(identical(output, prog))

  return(output)
}

# Example 
run(729, 0, 0, c(0,1,5,4,3,0))

# Real
run(51571418, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0))


## Part 2 ----------------

# Firstly commented out all the prints in the run function, and the delay

#example program...
run(2024, 0 , 0, c(0,3,5,4,3,0))
run(117440, 0 , 0, c(0,3,5,4,3,0))


# "What is the lowest positive initial value for register A that causes the program to output a copy of itself?"
# This is day 11 ... which suggests this is going to be3 a BIG number!
# But let's at least start off with a brute force

for (a in 1:1000000000) {
  if (identical(run(a, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0)),
                c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0))) {
    print(a)
    break
  }
}


# Hmmm. That might take a while!!
# Can make the function more efficent by instead returning a TRUE/FALSE answer
# if it's run in "part2" mode. It can then bale out as soon as the output
# begins to diverge from the inpit, rather that running the entire program

run(2024, 0 , 0, c(0,3,5,4,3,0), part2=T)
run(117440, 0 , 0, c(0,3,5,4,3,0), part2=T)

for (a in 1:1000000000) {
  if (run(a, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0), part2=T)) {
    print(a)
    break
  }
}


## Hmm... Not fast enough still
## Next ... Looking through the codeset, I suspect there are only
## certain starting conditions which will return a "2" as the first output
## Maybe there is a pattern to it...?

for (a in 1:100) {
  if (identical(run(a, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0))[1], c(2))) {
    print(a)
  }
}

# Hmmm not an obvious one.
# ANyway, it's lunch time. Let's go back and leave the brute-force test running....



## This is the chain of ops for the first output...
b = A mod 8
b = b xor 1
c = a / 2^b
a = a / 8
b = b xor 4
b = b xor c
output b mod 8

#which boils down to
bitwXor(bitwXor(bitwXor(a%%8,1),4),a%/%(2^bitwXor(a%%8,1)))%%8

# proof
for (a in 1:100) {
  if(bitwXor(bitwXor(bitwXor(a%%8,1),4),a%/%(2^bitwXor(a%%8,1)))%%8 == 2)
    print(a)
}


## This is a lot quicker, but still quite brute-forcish!
for (a in 1:1000000000) {
  if(bitwXor(bitwXor(bitwXor(a%%8,1),4),a%/%(2^bitwXor(a%%8,1)))%%8 == 2)
    if (run(a, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0), part2=T)) {
    print(a)
    break
  }
}
#Hmmm.. bitwXor won't even cope with these numbers!

# still no joy at 247842858


# How much Does length of output depend on size of input?

o <- 0
for (a in 1:1000000000) {
  l <- length(run(a, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0)))
  if (l > o) {
    print (c(l,a))
    o <- l
    Sys.sleep(1)
  }
}
# woot! these go up in powers of 8!
# Suggest won't get a solution before 8^15


for (a in (8^15):(8^16)) {
  if(bitwXor(bitwXor(bitwXor(a%%8,1),4),a%/%(2^bitwXor(a%%8,1)))%%8 == 2)
    if (run(a, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0), part2=T)) {
      print(a)
      break
    }
}


# Hmm bitwise xor doesn't go this big anyway :(
# So let's build our own. I think in reality there are only one big number
# in the mix, so can mod that to pull out the lower bits

bxo <- function(a,b) {
  bitwXor(a%%(2^31),b%%(2^31))
}

options(scipen=20)

run2 <- function(a,b,c,prog, part2 = FALSE) {
  ip <- 1
  #ps <- c(adv,bxl,bst,NA,NA,out,NA,NA)
  output <- numeric()
  
  combo <- function() {
    op <- prog[ip + 1]
    if (op == 0) return (0)
    if (op == 1) return (1)
    if (op == 2) return (2)
    if (op == 3) return (3)
    if (op == 4) return (a)
    if (op == 5) return (b)
    if (op == 6) return (c)
  }
  
  while(ip < length(prog)) {
    #if (ip == 1) print(c(a,b,c))
    #print(paste(c("ip=",ip," exec",prog[ip+0:1]),collapse=" "))
    
    if (prog[ip] == 0) {  ## adv ##
      a <- floor(a / (2^combo()))
    } else if (prog[ip] == 1) {  ## bxl ##
      b <- bxo(b,prog[ip+1])
    } else if (prog[ip] == 2) {  ## bst ##
      b <- combo() %% 8
    } else if (prog[ip] == 3) {  ## jnz ##
      if (a != 0) { ip <- prog[ip+1]-1 }
    } else if (prog[ip] == 4) {  ## bxc ##
      b <- bxo(b,c)
    } else if (prog[ip] == 5) {  ## out ##
      output <- c(output,combo()%%8)
      if (part2) {
        # if part2 = TRUE, returns T/F rather than output vector
        # And in that case, can output FALSE as soon as the result *begins* to deviate = more efficent!
        if(!identical(output, c(prog,-99)[1:length(output)]))
          return(FALSE)
      }
    } else if (prog[ip] == 7) {  ## adv ##
      c <- floor(a / (2^combo()))
    } else{
      message(paste0("invalid op: ip=", ip))
      return()
    }
    ip <- ip + 2
    #Sys.sleep(0.1)
  }
  #print("complete")
  if (part2) 
    return(identical(output, prog))
  
  return(output)
}


## Interesting.... the tail digits rarely change!
for (a in (8^15):(8^16)) {
  if(bxo(bxo(bxo(a%%8,1),4),a%/%(2^bxo(a%%8,1)))%%8 == 2)
    if (run2(a, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0), part2=T)) {
      print(a)
      break
    }
}

# Let's see how often the last 12 digits change...?
a <- 8^15
a2 <- a
out2 <- 0
while (TRUE) {
  out <- run2(a, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0))[5:16]
  if (!identical(out, out2)) {
    print(c(a, a-a2))
    a2 <- a
  }
          
  out2 <- out
  
  a <- a + 1
}

# Interesting - the last 12 digits only change after every (a multiple of) 4096 values
# Therefor if for example, the last 12 digits for out first candidate 8^15 aren't right, we can jump on another 4096 !


a <- 8^15
while (TRUE) {
  if (identical(run2(a, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0))[5:16],
                 c(7,5, 0,3, 1,4, 4,5, 5,5, 3,0)))
    for (b in 0:4095)
      if (run2(a+b, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0), part2=T)) {
        print(a+b)
        print(run2(a+b, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0)))
        break
      }
  a <- a + 4096
}



## That was still slow - let's look at the last 11 digits... which it turns out change every 32768
a <- 8^15
a2 <- a
out2 <- 0
while (TRUE) {
  out <- run2(a, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0))[6:16]
  if (!identical(out, out2)) {
    print(c(a, a-a2))
    a2 <- a
  }
  
  out2 <- out
  
  a <- a + 1
}

#Let's just assume this increases by 8 for each digit!
a <- 8^15
while (TRUE) {
  if (identical(run2(a, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0))[9:16],
                c(1,4, 4,5, 5,5, 3,0))) {
    print(paste("try candidates at",a))
    for (b in 0:(8^8-1))
      if (run2(a+b, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0), part2=T)) {
        print(a+b)
        print(run2(a+b, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0)))
        break
      }
  }
  a <- a + 8^8
}


#AGH! Horrible, clunky solution - but that found the solution so hey...

