library(tidyverse)
install.packages("caret")
library(caret)
library(dslabs)
data(heights)



#define the outcome and the predictor
y <- heights$sex
x <- heights$height


#generating training and test set
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE )
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]


#guess the outcome
y_hat <- sample(c('Male',"Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))


#compare the accuracy
mean(y_hat == test_set$sex)


#compare the heights in males and females in out data set
heights %>% 
  group_by(sex) %>%
  summarise(mean(height), sd(height))


#now predict male if height is within 2 standard deviation of the avarage male
y_hat <- ifelse(x > 62,"Male","Female") %>% factor(levels = levels(test_set$sex))

mean(y == y_hat)


#examine the effect of 10 cutoffs
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})


accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x,"Male","Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

data.frame(cutoff,accuracy) %>%
  ggplot(aes(cutoff,accuracy)) +
  geom_point() +
  geom_line()

max(accuracy)


#which is the best cutoff value
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

read_mnist()

str(mnist_27)

#this line of code develops a prediction of male and female depeded on best cutoff
#then the 
y_hat <- ifelse(test_set$height > best_cutoff,"Male","Female") %>%
  factor(levels = levels(test_set$sex))

y_hat
y_hat <- factor(y_hat)
#Day 2 Class

y_hat

table(predicted = y_hat, actual = test_set$sex)


test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarise(accuracy = mean(y_hat == sex))
