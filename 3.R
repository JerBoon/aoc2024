

## part 1

input <- "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
input <- readLines ("data/3.dat")
input <- paste(input, collapse="/")
input


muls <- strsplit(input,"mul")[[1]]
muls

sum <- 0
for (i in grep("^\\([0-9]{1,3},[0-9]{1,3})", muls)) {
  pair <- as.numeric(strsplit((gsub("[^0-9]"," ",muls[i]))," ")[[1]][2:3])
  print(pair)
  sum <- sum + prod(pair)
}
sum


## part 2 ----------------------------------

input <- "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
input <- readLines ("data/3.dat")
input <- paste(input, collapse="/")

# this is the new bit - it just ....'s out anything between dont() and do() sections

input <- gsub("don't\\().*?do\\()", "....", paste(input,"do()",sep=""))

# everything else is the same

muls <- strsplit(input,"mul")[[1]]
muls

sum <- 0
for (i in grep("^\\([0-9]{1,3},[0-9]{1,3})", muls)) {
  pair <- as.numeric(strsplit((gsub("[^0-9]"," ",muls[i]))," ")[[1]][2:3])
  print(pair)
  sum <- sum + prod(pair)
}
sum
