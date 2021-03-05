####    Getting started with R

### Creating objects (vectors)

a <- 3.4156
b <- 2
f <- c("apple","pear","orange")
g <- grepl("e$", f)

### Consistency of datatype in a vector


num_char <- c(1, 2, 3, "a")
num_logical <- c(1, 2, 3, TRUE)
char_logical <- c("a", "b", "c", TRUE)
tricky <- c(1, 2, 3, "4")