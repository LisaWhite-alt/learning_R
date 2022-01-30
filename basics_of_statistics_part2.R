cars <- rbind(c(20, 15), c(11, 12), c(7, 9))
test <- chisq.test(cars)
test$p.value
test$observed
test$expected
test$residuals


data <- mtcars[,c("am", "vs")]

smart_test <-  function(x){
  x <- table(x)
  if (length(x[x < 5]) > 0) {
    return(fisher.test(x)$p.value )
  } else {
    return(c(chisq.test(x)$statistic, chisq.test(x)$parameter, chisq.test(x)$p.value))
  }
}

smart_test(data)


most_significant <-  function(x){
  col_names <- names(x)
  x[col_names] <- lapply(x[col_names], factor)
  p_values <- sapply(x, function(x) chisq.test(table(x))$p.value)
  result <- col_names[which(p_values == min(p_values))]
  return(result)
}

most_significant(test_data)


iris$important_cases <- factor(apply(iris[, 1:4], 1, function(x) 
  if (sum(x > apply(iris[, 1:4], 2, mean)) >= 3) "Yes" else "No"))


test_data <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))

get_important_cases <- function(x){
  x$important_cases <- factor(apply(x, 1, function(y) 
    if (sum(y > apply(x, 2, mean)) > ncol(x)/2) "Yes" else "No"), levels = c("No", "Yes"))
  return(x)
}


stat_mode <- function(x){
  w <- sort(unique(x))
  b <- table(x)
  result <-  w[which(b == max(b))]
  return(result)
}


max_resid <- function(x){
  col_names <- names(x)
  x[col_names] <- lapply(x[col_names], factor)
  x <- table(x)
  test_stdres <- chisq.test(x)$stdres
  test_stdres_max <- max(test_stdres)
  result <- which(test_stdres == test_stdres_max, arr.ind = T)
  result_finish <- c(rownames(test_stdres)[result[1]], 
                     colnames(test_stdres)[result[2]])
  return(result_finish)
}


obj <- ggplot(diamonds, aes(x = color, fill = cut)) +
  geom_bar(position=position_dodge())


get_coefficients <- function(dataset){
  test_data <- transform(test_data, x = factor(x), y = factor(y))
  fit <- glm(y ~ x, test_data, family = "binomial")
  c <- coef(fit)
  return(sapply(c, exp))
}

get_coefficients(test_data)


centered <- function(test_data, var_names){
  test_data[var_names] <- lapply(test_data[var_names], function (x) x = x - mean(x))
  return(test_data)
}

var_names = c("X4", "X2", "X1")
centered(test_data, var_names)


