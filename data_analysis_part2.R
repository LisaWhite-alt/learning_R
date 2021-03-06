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

make.fancy.teapot <- function(teapot.coords) {
  i.s <- seq(0, length(teapot.coords$x) - 2, 3)
  j.s <- seq(1, length(teapot.coords$x) - 1, 3)
  k.s <- seq(2, length(teapot.coords$x), 3)
  plot_ly(teapot.coords, x = ~x, y = ~y, z = ~z, i = ~i.s, j = ~j.s, k = ~k.s, type = "mesh3d")
}

library(ggplot2)


my_names <- function (dataset, names){
  v <- sapply(dataset$name, function(x) grepl(paste(names,collapse="|"), x))
  return(dataset[v,])
}


to_factors <- function(test_data, factors){    
  test_data[factors] <- mutate_each(test_data[factors], funs(factor(ifelse(. > mean(.), 1, 0))))    
  return(test_data)}

high_price <- diamonds %>%
  group_by(color) %>%
  arrange(desc(price)) %>%
  select(color, price) %>%
  slice(1:10)



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

library(dplyr)

d <- slice(diamonds, seq(1, nrow(diamonds), 2))

my_df <- mtcars %>% 
  select(mpg, hp, am, vs) %>% 
  filter(mpg > 14, hp > 100) %>% 
  arrange(desc(mpg)) %>% 
  slice(1:10) %>% 
  rename("Miles per gallon" = "mpg", "Gross horsepower" = "hp")

all_to_factor <- function(x){
  mutate_each(x, funs(factor(.)))
}

log_transform <- function(test_data){
  mutate_if(test_data, is.numeric, funs(log((.-min(.))/(max(.)-min(.))+1)))
}

descriptive_stats <- function (dataset){
  dataset$gender <- factor(dataset$gender)
  dataset$country <- factor(dataset$country)
  dataset %>%
    group_by(gender, country) %>%
    summarise(n = n(), 
              mean = mean(salary, na.rm = T),
              sd = sd(salary, na.rm = T),
              median = median(salary, na.rm = T),
              first_quartile = quantile(salary, na.rm = T, )[2],
              third_quartile = quantile(salary, na.rm = T, )[4],
              na_values = sum(is.na(salary)))
}

library(data.table)

filter.expensive.available <- function(products, brands) {
  products[(price >= 500000) & (available == TRUE) & (brand %in% brands)]
}

ordered.short.purchase.data <- function(purchases) {
  purchases[order(-price)][quantity >= 0, .(ordernumber, product_id)]
}

purchases.median.order.price <- function(purchases) {
  purchases[quantity >= 0, .(totalcents = sum(price * quantity)), by = ordernumber][, .(median(totalcents))][[1]]
}

get.category.ratings <- function(purchases, product.category) {
  setkey(product.category, product_id)
  setkey(purchases, product_id)
  dt <- merge(product.category, purchases, by = "product_id")
  result <- dt[, .(totalcents = sum(totalcents), quantity = sum(quantity)), by=category_id]
  return(result)
}

mark.position.portion <- function(purchases) {
  purchases[quantity >= 0][, price.portion := as.character(format(round(price*quantity/(sum(price*quantity))*100, 2), nsmall = 2)), by=ordernumber][]
}

fix_data <- function(d){
  fun <- function(vector) {
    res_vector <- c()
    for (x in vector) {
      y <- suppressWarnings(as.numeric(gsub(" ", "", x, fixed = TRUE)))
      if (is.na(y) == TRUE) return(vector)
      res_vector <- c(res_vector, y)
    }
    return(res_vector)
  }
  for (i in 1:ncol(d)) {
    d[, i] <- fun(d[, i])
  }
  return(d)
}

get_id <- function(data_list){
  for (i in 1:7) names(data_list[[i]])[2] <- paste0("temp", i)
  dt_r <- data.frame()
  dt_r <- merge(data_list[[1]], data_list[[2]], by = "id", all.x = T, all.y = T)
  for (i in 3:7) dt_r <- merge(dt_r, data_list[[i]], by = "id", all.x = T, all.y = T)
  dt_r$mean_temp <- rowMeans(dt_r[,2:8])
  dt_r[!is.na(dt_r$mean_temp),][, c(1, 9)]
}

get_strange_var <- function(d){
  a <- lm(x ~ t, d)
  b <- lm(x ~ z, d)
  c <- lm(t ~ z, d)
  res1=stats::rstandard(a)
  a_p <- shapiro.test(res1)$p.value
  res2=stats::rstandard(b)
  b_p <- shapiro.test(res2)$p.value
  res3=stats::rstandard(c)
  c_p <- shapiro.test(res3)$p.value
  if (a_p < 0.05 & b_p < 0.05) return("x")
  if (b_p < 0.05 & c_p < 0.05) return("z")
  if (c_p < 0.05 & a_p < 0.05) return("t")
  return("There is no strange variable in the data")
}

library(ggplot2)

depth_hist <- qplot(x = depth, data = diamonds)

price_carat_clarity_points <- qplot(x = carat, 
                                    y = price, 
                                    color = factor(clarity), 
                                    data = diamonds)

x_density <- qplot(x = x, 
                   data = diamonds,
                   geom = "density")
x_cut_density <- qplot(x = x,
                       color = factor(cut),
                       data = diamonds,
                       geom = "density")

price_violin <- qplot(x = factor(color),
                      y = price,
                      data = diamonds,
                      geom = "violin" )

my_plot <- ggplot(mtcars, aes(factor(am), mpg)) +
  geom_violin() +
  geom_boxplot(width = 0.2)

my_plot <- ggplot(sales, aes(income, sale)) + 
  geom_point(aes(col = shop), size = 3) + 
  geom_smooth(formula = y ~ x, method = "lm")

my_plot <- ggplot(sales, aes(shop, income, col = season))+
  stat_summary(fun.data = mean_cl_boot,
               geom = "pointrange",
               position = position_dodge(0.2))

my_plot <-  ggplot(sales, aes(date, sale, col = shop)) +
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(0.2)) + 
  stat_summary(fun.data = mean_cl_boot,
               geom = "point", 
               size = 2, 
               position = position_dodge(0.2)) + 
  stat_summary(fun.data = mean_cl_boot,
               geom = "line", 
               position = position_dodge(0.2)) 

mpg_facet <- ggplot(mtcars, aes(mpg)) +
  geom_dotplot() + 
  facet_grid(am ~ vs)

sl_wrap <- ggplot(iris, aes(Sepal.Length)) +
  geom_density() +
  geom_hline(yintercept = 0) + 
  facet_wrap(~ Species)

my_plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
  geom_point() +
  geom_smooth() + 
  facet_wrap(~ Species)

my_plot <- ggplot(myMovieData, aes(Type, Budget)) +
  geom_boxplot() +
  facet_grid(. ~ Year) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

iris_plot <- ggplot(iris, aes(Sepal.Length, Petal.Length, col = Species))+
  geom_point() +
  geom_smooth(method = "lm")+
  scale_color_discrete(name = "��� ������", 
                       labels = c("���� ����������", "���� ������������", "���� ����������"))+
  scale_x_continuous(name = "����� �����������",
                     breaks = seq(4, 8, 1),
                     limits = c(4, 8))+ 
  scale_y_continuous(name = "����� ��������",
                     breaks = seq(1, 7, 1))

library("rmarkdown")

df <- read.csv("glacier.csv", na.strings = "..", comment.char = "#")

df$GEO <- factor(df$GEO)
library(dplyr)
gr_df <- group_by(df, GEO, Ref_Date)
se_df <- summarise(gr_df, Count = n())
ct <- group_by(se_df, GEO)
dt <- summarise(ct, Count = n())
min_count <- min(dt$Count)
a <- dt[dt$Count == min_count,][1]

df_annual <- df[df$MEASURE == "Annual mass balance",]
df_annual_gr <- group_by(df_annual, GEO)
df_annual_gr_sum <- summarise(df_annual_gr, median=median(Value, na.rm = T))
min_count_med <- min(abs(df_annual_gr_sum$median))
b <- df_annual_gr_sum[abs(df_annual_gr_sum$median) == min_count_med,][1]

c <- df[is.na(df$Value),][2]





