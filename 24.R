


dat <- readLines("data/24.test.dat")
dat <- readLines("data/24.dat")

vals <- dat[1:(which(dat == "")-1)]
rules <- dat[(which(dat == "")+1):length(dat)]

rules <- as.data.frame( matrix(unlist(strsplit(rules,"[^[:alnum:]]+")),ncol=4, byrow=T))
names(rules) <- c("g1","op","g2","res")
vals <- as.data.frame( matrix(unlist(strsplit(vals,"[^[:alnum:]]+")),ncol=2, byrow=T), )
names(vals) = c("gate","value")


## Part 1 ------------------------------

gates <- merge(data.frame(gate = sort(unique(c(rules$g1,rules$g2, rules$res)))), vals, all.x=T)
rules$solved <- FALSE

while (sum(!rules$solved) > 0) {
  print(sum(!rules$solved))
  for (i in 1:nrow(rules)) {
    rules$v1[i] <- as.integer(gates$value[gates$gate==rules$g1[i]])
    rules$v2[i] <- as.integer(gates$value[gates$gate==rules$g2[i]])
  }

  for (i in which(!rules$solved & !is.na(rules$v1) & !is.na(rules$v2))) {
    if (rules$op[i] == "AND") {
      gates$value[ gates$gate == rules$res[i] ] <- as.integer(rules$v1[i] & rules$v2[i])
    }
    if (rules$op[i] == "OR") {
      gates$value[ gates$gate == rules$res[i] ] <- as.integer(rules$v1[i] | rules$v2[i])
    }
    if (rules$op[i] == "XOR") {
      gates$value[ gates$gate == rules$res[i] ] <- as.integer(rules$v1[i] != rules$v2[i])
    }
    rules$solved[i] <- TRUE
  }
}

paste(rev(gates$value[grep("^z",gates$gate)]), collapse="")
strtoi(paste(rev(gates$value[grep("^z",gates$gate)]), collapse=""), base=2)   # Won't work on real input

#strtoi can't handle integers as big as the real answer so need to do it manually
options(scipen=20)
sum(as.numeric(gates$value[grep("^z",gates$gate)]) * (2 ^ ((1:sum(grepl("^z",gates$gate)))-1)))


## Part 2 --------------------------------------

