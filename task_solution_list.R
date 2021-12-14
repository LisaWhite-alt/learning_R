# get the longest element
get_longest <- function(l) {
  len <- sapply(l, length)
  ind <- which.max(len)
  list(number = ind, element = l[[ind]])
}

# generate list with random and contents
gen_list <- function(n_elements, max_len, seed = 111) {
  set.seed(seed)
  len <- sample(1:max_len, n_elements)
  lapply(len, rnorm)
}

l1 <- gen_list(4, 10, 888)
l1
gl1 <- get_longest(l1)
gl1$number


# count different elements in vector
count_elements <- function(x) {
  a <- unique(x)
  a <- a[order(a)]
  b <- sapply(a, function(i) length(which(x == i)))
  m <- matrix(c(a, b), nrow = 2, byrow = T)
}

x <- c(5, 2, 7, 7, 7, 2, 0, 0)
y <- count_elements(x)
y


