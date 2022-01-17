get_negative_values <- function(test_data){
  result_list <- apply(test_data, 2, function(x) x[!is.na(x) & x < 0])
  result_true <- sapply(result_list, function(x) length(x) != 0)
  result <- result_list[result_true]
  result_length <- unique(sapply(result, length))
  if (length(result_length) == 1) result <- as.data.frame(result)
  return(result)
}

na_rm  <- function(x){
  change <- function(y) {
    y[is.na(y)] <- mean(y, na.rm = T)
    return(y)
  }
  result <- apply(x, 2, change)
  return(as.data.frame(result))
}

positive_sum <-  function(test_data){
  sum_pos <- function(x){
    x[is.na(x) | x < 0] <- 0
    return(sum(x))
  }
  lapply(as.list(test_data), sum_pos)
}

my_names <- function (dataset, names){
  v <- sapply(dataset$name, function(x) grepl(paste(names,collapse="|"), x))
  return(dataset[v,])
}

find_outliers <- function(t){
  number <- which(sapply(t, is.numeric) == T)
  t$mean <- ave(t[, number], t[, -number], FUN=mean)
  t$sd <- ave(t[, number], t[, -number], FUN=sd)
  t$is_outlier <- ifelse(abs(t[, number] - t[, ncol(t)-1]) > (2 * t[, ncol(t)]), 1, 0)
  return(t[,-((ncol(t)-2):(ncol(t)-1))])
}

smart_lm <- function(x){
  norm_v <- sapply(x[-1], function(y) shapiro.test(y)$p.value > 0.05)
  if (sum(norm_v == TRUE) == 0) return(print("There are no normal variables in the data"))
  norm_v <- c(F, norm_v)
  c <- paste(colnames(x[norm_v == TRUE]), collapse = " + ")
  c <- paste(c(colnames(x[1]), c), collapse = " ~ ")
  fit <- lm(as.formula(c), data = x)
  return(fit$coefficients)
}

one_sample_t <- function(test_data, general_mean){
  df <- test_data[,sapply(test_data, is.numeric)]
  b <- lapply(df, function(y) {
    fit <- t.test(y, mu=general_mean)
    return(c(fit$statistic, fit$parameter, fit$p.value))
  })
  return(b)
}

get_p_value <- function(test_list){
  return(lapply(test_list, function(y) y$p.value))
}

d <- slice(diamonds, seq(1, nrow(diamonds), 2))

my_df <- mtcars %>% 
  select(mpg, hp, am, vs) %>% 
  filter(mpg > 14, hp > 100) %>% 
  arrange(desc(mpg)) %>% 
  slice(1:10) %>% 
  rename("Miles per gallon" = "mpg", "Gross horsepower" = "hp")

