# max diff neighbor
maxdiff <- function(x) {
  y <- abs(x[-1] - x[-length(x)])
  k <- which(y == max(y))
  print("First neighbor(s):")
  print(x[k])
  print("Second neighor(s):")
  print(x[k+1])
  print("Maximum absolute diff is:")
  print(max(y))
}

xx <- sample(1:100, 10000, replace = TRUE)
maxdiff(xx)


# is monotone vector
is_monotone <- function(x) {
  y <- x[-1] - x[-length(x)]
  if (all(y <= 0)) {
    print(TRUE)
  } else if (all(y >= 0)) {
    print(TRUE)
  } else {
    print(FALSE)
  }
}

xx <- c(0, 0, 3, 4, 4, 8)
is_monotone(xx)
xx <- c(10, 9, 6, 6, 4, 2)
is_monotone(xx)
xx <- c(0, 4, 13, 24, 4, 8)
is_monotone(xx)


# combin count 
combin_count <- function(n, k, with_repretitions = FALSE) {
  if (with_repretitions == FALSE) {
    print(factorial(n)/(factorial(k)*factorial(n-k)))
  } else {
    print(factorial(n+k-1)/(factorial(k)*factorial(n-1)))
  }
}

combin_count(6, 2)
combin_count(6, 2, TRUE)
