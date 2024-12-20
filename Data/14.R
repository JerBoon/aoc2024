

dat <- readLines("data/14.test.dat")
width <- 11
height <- 7

dat <- readLines("data/14.dat")
width <- 101
height <- 103


df <- data.frame()
for (i in 1:length(dat))
  df[i,c("x","y","vx","vy")] <- as.numeric(strsplit(dat[i],"[vp= ,]+")[[1]][2:5])
df



## Part 1

# This seems too easy....? Part 2 is going to be "fun" :)

move <- function(df) {
  df$x <- (df$x + df$vx)%%width
  df$y <- (df$y + df$vy)%%height
  return(df)
}

for (i in 1:100)
  df <- move(df)
df

prod(table(sign(df$x - width%/%2),sign(df$y - height%/%2))[c(1,3),c(1,3)])


## Part 2

# Ooh horrible - this is so subjective... can I even look for a patternm in code..?
# What is "most of the robots" what is "shape of a christmas tree"....

# So many thoughts... 101x103 is tricky to output as text ... and wouldn't want to scroll through
# What about plots/charts - save a files. Can maybe scroll through thumbnails....

library(ggplot2)
library(dplyr)

for (i in 1:1000) {
  df <- move(df)
  g <- df %>% ggplot(aes(x=x,y=y)) + geom_point()
  ggsave(sprintf("outputs/sec %010d.png",i), g,width=4,height=4)
}

# Hmmmm... what's a wait to look for data that looks less random..?

for (i in 1:100000) {
  df <- move(df)
  if(sd(df$x) < 25 & sd(df$y) < 25) {
    print(c(i,sd(df$x), sd(df$y)))
    g <- df %>% ggplot(aes(x=x,y=y)) + geom_point()
    ggsave(sprintf("outputs/sec %010d.png",i), g,width=4,height=4)
  }
}
