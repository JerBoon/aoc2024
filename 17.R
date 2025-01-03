


## It's a trap!!
## There are only 8 instructions in the program, so why not just hard wire the code....?
## Gonna do that for part 1, and then see what shenanegans part 2 have in store to
## render this method obsolete....


## Part 1 -----------


run <- function(a,b,c,prog) {
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
    print(paste(c("ip=",ip," exec",prog[ip+0:1]),collapse=" "))
    
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
    } else if (prog[ip] == 7) {  ## adv ##
        c <- floor(a / (2^combo()))
    } else{
      message(paste0("invalid op: ip=", ip))
      return()
    }
    ip <- ip + 2
    Sys.sleep(0.1)
  }
  print("complete")
  print(paste(output,collapse=","))
}

# Example 
run(729, 0, 0, c(0,1,5,4,3,0))

# Real
run(51571418, 0, 0, c(2,4, 1,1, 7,5, 0,3, 1,4, 4,5, 5,5, 3,0))

