month_7_8_9 <- subset(airquality, airquality$Month %in% c(7, 8 ,9),)
result <- aggregate(Ozone ~ Month, month_7_8_9, length)

stat3 <- describe(x = iris)

stat4 <- describeBy(x = iris, group = iris$Species, mat = T)

my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA
fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))

ggplot(airquality, aes(x = Month, y = Ozone, group = Month)) + geom_boxplot()
boxplot(Ozone ~ Month, airquality)

plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, col = hp)) + geom_point()

ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))
ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()

plot2 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                          col = Species, size = Petal.Length)) + geom_point()

red_men <- prop.table(HairEyeColor[, "Blue", "Male"])["Red"]
sum(HairEyeColor[,"Green","Female"])

library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
obj <- ggplot(data = mydata[mydata$Sex == "Female",], aes(x = Hair, y = Freq, fill = Eye)) + 
geom_bar(stat="identity", position = "dodge") +
scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

main_stat <- chisq.test(table(diamonds$cut, diamonds$color))$statistic

diamonds$factor_price <- 0
diamonds$factor_price[diamonds$price >= mean(diamonds$price)] <- 1
diamonds$factor_price <- factor(diamonds$factor_price)
diamonds$factor_carat <- 0
diamonds$factor_carat[diamonds$carat >= mean(diamonds$carat)] <- 1
diamonds$factor_carat <- factor(diamonds$factor_carat)
main_stat <- chisq.test(table(diamonds$factor_price, diamonds$factor_carat))$statistic

df <- ToothGrowth
df1 <- subset(df, (supp == "OJ" & dose == 0.5) | (supp == "VC" & dose == 2))
t_stat <- t.test(len ~ supp, df1)$statistic

df <- read.csv("lekarstva.csv")
t_stat <- t.test(df$Pressure_before, df$Pressure_after, paired = T)

df <- read.table("dataset_11504_15.txt")
bartlett.test(V1 ~ V2, df)
paired_wtest  <- wilcox.test(V1 ~ V2, df)
paired_ttest <- t.test(V1 ~ V2, df, var.equal = T)

df <- npk
fit <- aov(yield ~ N * P, data = df)
summary(fit)

df <- iris
fit <- aov(Sepal.Width ~ Species, data = df)
TukeyHSD(fit)
ggplot(df, aes(x = Species, y = Sepal.Width))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar')

df2 <- read.csv("Pillulkin.csv")
df2$patient <- factor(df2$patient)
fit2 <- aov(temperature ~ pill * doctor + Error(patient/(pill*doctor)), data = df2)
summary(fit2)

obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj


NA.position <- function(x){    
  which(is.na(x))}

NA.counter <- function(x){    
  return(sum(is.na(x)))}

outliers.rm <- function(x){
  my_iqr <- IQR(x)
  my_quant <- quantile(x, probs = c(0.25, 0.75))
  my_del <- which((x - my_quant[2] > my_iqr * 1.5) | (my_quant[1] - x > my_iqr * 1.5))
  x <- x[-my_del]
  return(x)
}

corr.calc <- function(x){
  fit <- cor.test(x = x[,1], y = x[,2])
  return(c(fit$estimate, fit$p.value))
}

filtered.cor <- function(x){
  library(psych)
  x <- x[, sapply(x, is.numeric)]
  fit  <- corr.test(x)$r
  diag(fit) <- 0
  i <- which.max(abs(fit))
  result <- fit[i]
  return(result)
}

smart_cor <- function(x){
  sh_test_1 <- shapiro.test(x[, 1])$p.value
  sh_test_2 <- shapiro.test(x[, 2])$p.value
  if (sh_test_1 < 0.05 | sh_test_2 < 0.05) {
    result <- cor.test(x[, 1], x[, 2], method = "spearman")$estimate
  } else {
    result <- cor.test(x[, 1], x[, 2], method = "pearson")$estimate
  }
  return(result)
}

dataframe <-  read.table("dataset_11508_12.txt", sep=' ' )
fit  <- lm(V1 ~ V2, dataframe)
summary(fit)
fit$coefficients

regr.calc <- function(x){
  p <- cor.test(x[, 1], x[, 2], method = "pearson")$p.value
  if (p < 0.05) {
    fit  <- lm(x[, 1] ~ x[, 2], x)
    x$fit <- fit$fitted.values
    return(x)
  } else {
    return("There is no sense in prediction")
  }
}

my_plot <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, col = Species))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")

fill_na <- function(x){
  fit <- lm(y ~ x_1 + x_2, x, )
  x$y_full <- predict(fit, x)
  x$y_full <- ifelse(is.na(x$y), x$y_full, x$y)
  return(x)
}

model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)
ideal_model <- step(object = model_null,
                    scope = list(lower = model_null, upper = model_full),
                    direction = "forward")
anova(model_full, ideal_model)

