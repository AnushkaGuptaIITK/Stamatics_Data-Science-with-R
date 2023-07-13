
#####################question1

library(ggplot2)
data(iris)
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species))+ 
  geom_point()+ labs(x="Sepal Length", y="Petal Length", title="Scatterplot")

####################question2
library(ggplot2)
data(txhousing)

str(txhousing)

summary(txhousing)

head(txhousing)

missing_values <- sum(!complete.cases(txhousing))
cat("Number of missing values:", missing_values, "\n")


ggplot(txhousing, aes(x = year, y = sales)) +
  geom_point() +
  labs(x = "year", y = "sales", title = "Scatterplot of year vs. sales")

ggplot(txhousing, aes(x = month, y = sales)) +
  geom_point() +
  labs(x = "month", y = "sales", title = "Scatterplot of month vs. sales")



ggplot(txhousing, aes(x = city, y = sales)) +
  geom_boxplot() +
  labs(x = "city", y = "sales", title = "Boxplot of sales by city")


######################question3

library(ggplot2)
titanic <- read.csv("titanic.csv")

finalP <- ggplot(titanic, aes(x = Fare, y = Survived, color = factor(Sex))) +
  geom_boxplot() +
  labs(x = "Fare", y = "Survived", title = "Fare vs Survival", color = "Sex") +
  scale_color_manual(values = c("#FFC0CB", "#87CEFA"), labels = c("Female", "Male"))


finalP


