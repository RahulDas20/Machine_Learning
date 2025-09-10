library(dslabs)
mnist <- read_mnist()

y <- mnist$train$labels

library(tidyverse)
library(caret)
data(heights)

y <- heights$sex
x <- heights$height


#A standard way of splitting data
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
#times define how many random samples of index to return
#p defines the proportion of data from the actual dataset
#list is used to decide if we want the index as a list


test_index
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]


y_hat <- sample(c("Male","Female"), length(test_index), replace = TRUE) |>
  factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)


heights |> group_by(sex) |> summarise(mean(height), sd(height))

#lets plot a function which will predict male when the height is more than 62

y_hat <- ifelse(x > 62, "Male","Female") |>
  factor(levels = levels(test_set$sex))

mean(y_hat == y)
#here we can see that our accuracy is improved when we used a specific height
#but can we do better?

#we can use 10 different cutoffs when optimizing our height

cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") |>
    factor(levels = levels(test_set$sex))
  mean(y_hat == test_set$sex)
})

data <- data.frame(cutoff,accuracy)
data |> ggplot(aes(cutoff, accuracy)) +
  geom_line()

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]

best_cutoff

y_hat <- ifelse(train_set$height > best_cutoff,"Male", "Female") |>
  factor(levels = levels(train_set$sex))
mean(y_hat == train_set$sex)

#this proves that if we use cutoffs we can improve the accuracy of our model
table(predicted = y_hat, actual = train_set$sex)

train_set |>
  mutate(y_hat = y_hat) |>
  group_by(sex) |>
  summarise(accuracy = mean(y_hat == sex))

#there is an imbalance in the accuracy between male and female
prev <- mean(y == "Male")
prev
#sensitivity is defined as the ability of an algorithm to predict a positive outcome when the actual outcome is positive
#specificity is defined as the ability of an algorithm to predict a negetive outcome when the actual outcome is negetive

#balanced accuracy is the average of specificity and sensitivity


#lets build our algorithm once again
cutoff <- seq(61,70)
f_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") |>
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

plot(cutoff, f_1)

max(f_1)

#this maximum is achieved when we use the following cutoff
best_cutoff <- cutoff[which.max(f_1)]
best_cutoff


y_hat <- ifelse(test_set$height > 66,"Male", "Female") |>
  factor(levels = levels(test_set$sex))

sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

# it means this algorithm can predict a positive when the result is actually positive about 40% and 
# but it can predict negatives about 90% of the time

 