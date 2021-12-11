# find closest
find_closest <- function(v, n) {
  m <- matrix(c(v, rep(n, length(v))), ncol = 2)
  y <- abs(m[, 1] - m[, 2])
  l <- which(y == min(y))
  return(l)
}

print(find_closest(c(5, 2, 7, 7, 7, 2, 0, 0), 1))


# bind matrices diagonally
bind_diag <- function(m1, m2, fill) {
  m3 <- matrix(fill,
               nrow = nrow(m1) + nrow(m2),
               ncol = ncol(m1) + ncol(m2)
  )
  m3[1:nrow(m1), 1:ncol(m1)] <- m1
  m3[(nrow(m1)+1):(nrow(m1)+nrow(m2)), (ncol(m1)+1):(ncol(m1)+ncol(m2))] <- m2
  return(m3)  
}

m1 <- matrix(1:12, nrow = 3)
m2 <- matrix(10:15, ncol = 3)
print(bind_diag(m1, m2, fill = NA))
print(bind_diag(m1, m2, fill = 0))