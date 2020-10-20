# Simple Linear Regression

# Importing the dataset
dataset = read.csv('student_scores.csv')


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Scores, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
 training_set = scale(training_set)
 test_set = scale(test_set)
training_set = as.data.frame(training_set)
test_set = as.data.frame(test_set)
# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Scores ~ Hours,
               data = training_set)


# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
predict(regressor, newdata = data.frame(Hours= 9.25))

# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$Hours, y = training_set$Scores),
             colour = 'red') +
  geom_line(aes(x = training_set$Hours, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Score vs Hours (Training set)') +
  xlab('hours') +
  ylab('Score')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$Hours, y = test_set$Scores),
             colour = 'red') +
  geom_line(aes(x = training_set$Hours, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Hours vs Score (Test set)') +
  xlab('Hours') +
  ylab('Salary')