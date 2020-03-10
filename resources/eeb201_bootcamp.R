library("swirl")
library(dplyr)
#swirl()

#iris dataset
iris_ds <- iris
setosa_data <- iris_ds %>% 
  filter(Species == "setosa")

virginica_data <- iris_ds %>%
  filter(Species == "virginica")

#plot sepal lengs of the two species 
hist(setosa_data$Sepal.Length)
hist(virginica_data$Sepal.Length)

plot(setosa_data$Sepal.Length)

t.test(setosa_data$Sepal.Length, virginica_data$Sepal.Length)
aov.out <- aov(virginica_data$Sepal.Length ~ setosa_data$Sepal.Length)
aov.out


lower_range <- 1
upper_range <- 100
sum_square <- 0
for(ii in lower_range:upper_range)
{
  cat("\n",ii," ",ii^2)
  sum_square = ii^2 + sum_square
}
cat("here is the sum of it all", sum_square, " squaresum")


RR = 1.05
NN = 100
tt = 10
for(ii in 1:tt)
{
  cat("\n",RR*NN*ii)
}
