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


get_features <- function(dataset){
  test_data <- transform(test_data, is_prohibited = factor(is_prohibited), 
                         type = factor(type))
  fit <- glm(is_prohibited ~ weight + length + width + type, test_data, 
             family = "binomial")
  result <- anova(fit, test = "Chisq")
  result_var <- rownames(result)[2:nrow(result)][result[2: nrow(result), 
                                                        "Pr(>Chi)"] < 0.05]
  if (length(result_var) == 0) return("Prediction makes no sense")
  return(result_var)
}

most_suspicious <- function(test_data, data_for_predict){
  test_data <- transform(test_data, is_prohibited = factor(is_prohibited), 
                         type = factor(type))
  fit <- glm(is_prohibited ~ weight + length + width + type, test_data, 
             family = "binomial")
  data_for_predict$corr  <- predict(fit, newdata = data_for_predict, type = "response")
  result <- data_for_predict$passangers[which(data_for_predict$corr == max(data_for_predict$corr))]
  return(result)
}


normality_test <- function(dataset){
  data <- dataset[sapply(dataset, is.numeric)]
  result <- sapply(data, function (x) shapiro.test(x)$p.value)
  return(result)
}


smart_anova <- function(test_data){
  test_data$y <- factor(test_data$y)
  sh <- aggregate(test_data$x, by = list(test_data$y), function(x) shapiro.test(x)$p.value)
  ba <- bartlett.test(test_data$x, test_data$y)$p.value
  if (all(sh$x >= 0.05) & (ba >= 0.05)) {
    fit <- aov(x ~ y, data = test_data)
    p_value <- c("ANOVA" = summary(fit)[[1]]$'Pr(>F)'[1])
  } else {
    fit <- kruskal.test(x ~ y, data = test_data)
    p_value <- c("KW" = fit$p.value)
  }
  return(p_value)
}

library(dplyr)
normality_by <- function(test){
  result <- data.frame(test %>%
  group_by(y, z) %>%
  summarise(p_value = shapiro.test(x)$p.value))
  return(result)
}


obj <- ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_density(alpha = 0.2)


smart_hclust <- function(test_data, cluster_number){
  dist_matrix <- dist(test_data)
  fit <- hclust(dist_matrix)
  cluster <- cutree(fit, cluster_number)
  test_data$cluster <- factor(cluster)
  return(test_data)
}


get_difference <- function(test_data, n_cluster){
  dist_matrix <- dist(test_data)
  fit <- hclust(dist_matrix)
  cluster <- cutree(fit, n_cluster)
  test_data$cluster <- factor(cluster)
  p_values <- sapply(test_data[-ncol(test_data)], function(x) summary(aov(x ~ cluster, test_data)))
  result <- sapply(p_values, function(x) x$`Pr(>F)`[1] < 0.05)
  return(names(test_data[-ncol(test_data)])[result])
}


get_pc <- function(d){
  result <- prcomp(d)$x
  d$PC1 <- result[, 1]
  d$PC2 <- result[, 2]
  return(d)
}


get_pca2 <- function(data){
  fit <- prcomp(data)
  result <- summary(fit)$importance[3, ]
  number <- min(which(result > 0.9))
  data <- cbind(data, fit$x[, 1:number])
  return(data)
}


swiss <- smart_hclust(swiss, 2)
library(ggplot2)
my_plot <- ggplot(swiss, aes(Education, Catholic, col = cluster)) +
  geom_point() + geom_smooth(method = "lm")


library(dplyr)

is_multicol <- function(d){
  result <- c()
  for (i in 1:(ncol(d)-1)) {
    for (j in (i+1):ncol(d)) {
      model <- lm(d[, j] ~ d[, i])
      if (near(abs(model$coefficients[2]), 1)) result <- c(result, names(d)[j], names(d)[i])            
    }
  }
  if (length(result) == 0) return("There is no collinearity in the data") else return(result)
}




  

