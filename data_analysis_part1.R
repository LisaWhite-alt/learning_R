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

