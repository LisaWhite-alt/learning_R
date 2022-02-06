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


plot_1 <- ggplot(exp_data, aes(scenario, frequency, fill = attitude)) +
  geom_boxplot(position=position_dodge())


plot_2 <- ggplot(exp_data, aes(frequency, fill = subject)) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~ gender, nrow = 2)


fit_1 <- lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=exp_data)


fit_2 <- lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=exp_data)


fit_3 <- lmer(frequency ~ attitude + gender + 
                (1 + attitude|subject) + (1 + attitude|scenario), data=exp_data)


median_cl_boot <- function(x){
  med_x <- median(x)
  delta <- c()
  for (i in 1:1000) {
    y <- sample(x, length(x), replace = T)
    med_y <- median(y)
    delta <- c(delta, med_x-med_y)
  }
  q <- quantile(delta, probs = c(0.05, 0.95))
  return(c(q[1]+med_x, q[2]+med_x))
}


slope_cl_boot <- function(x){
  coef_x <- cor(x[, 2], x[, 1])
  delta <- c()
  for (i in 1:1000) {
    y <- x[sample(nrow(x), nrow(x), replace = T), ]
    coef_y <- cor(y[, 2], y[, 1])
    delta <- c(delta, coef_x-coef_y)
  }
  q <- quantile(delta, probs = c(0.05, 0.95))
  return(c(q[1]+coef_x, q[2]+coef_x))
}





