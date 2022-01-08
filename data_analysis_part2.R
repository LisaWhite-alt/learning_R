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

