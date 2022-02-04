hetero_test <-  function(test_data){
  fit1 <- lm(test_data[, 1] ~ ., test_data[, -1])
  fit2 <- lm(fit1$residuals ^ 2 ~ ., test_data[, -1])
  return(summary(fit2)$r.squared)
}


VIF <-  function(test_data){
  data <- test_data[, -1]
  result <- c()
  for (i in 1:ncol(data)) {
    fit <- lm(data[, i] ~ ., as.data.frame(data[, -i]))
    r2 <- summary(fit)$r.squared
    vif_x <- 1 / (1 - r2)
    result <- c(result, vif_x)
  }
  names(result) <- names(data)
  return(result)
}


VIF <-  function(data){
  result <- c()
  for (i in 1:ncol(data)) {
    fit <- lm(data[, i] ~ ., as.data.frame(data[, -i]))
    r2 <- summary(fit)$r.squared
    vif_x <- 1 / (1 - r2)
    result <- c(result, vif_x)
  }
  return(result)
}

smart_model <-  function(test_data){
  data <- test_data[, -1]
  while (ncol(data) > 1) {
    result <- VIF(data)
    if (sum(result > 10) == 0) {
      fit <- lm(test_data[, 1] ~ ., data)
      return(fit$coef)
    } else {
      if (ncol(data) == 2) {
        fit <- lm(test_data[, 1] ~ data[, 2])
        names(fit$coef) <- c("(Intercept)", names(data)[2])
        return(fit$coef)
      } else {
        index <- which(result == max(result))
        data <- data[, -index]
      }
    }
  }
}


transform_x <-  function(test_data){
  result_coef <- c()
  result_x <- list()
  for (lambda in seq(-2, 2, 0.1)) {
    if (lambda > 0) {
      x_lambda <- function(x) x ^ lambda
    } else {
      if (lambda == 0) {
        x_lambda <- function(x) log(x)
      } else {
        x_lambda <- function(x) -(x ^ lambda)
      }
    }
    x_new <- x_lambda(test_data$x)
    result_x <- c(result_x, list(x_new))
    coef_lambda <- cor(test_data$y, x_new)
    result_coef <- c(result_coef, abs(coef_lambda))
  }
  index <- which.max(result_coef)
  return(result_x[[index]])
}






