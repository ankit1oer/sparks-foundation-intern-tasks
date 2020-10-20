# Decision Tree Classification

# Importing the dataset
dataset = read.csv('Iris.csv')
dataset = dataset[2:6]

summary(dataset)

# Encoding the target feature as factor
dataset$Species= factor(dataset$Species,
                           levels = c('Iris-setosa','Iris-versicolor','Iris-virginica'),
                           labels = c(0, 1,2))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Species, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-5] = scale(training_set[-5])
test_set[-5] = scale(test_set[-5])

# Fitting Decision Tree Classification to the Training set
# install.packages('rpart')
library(rpart)
classifier = rpart(formula = Species ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-5], type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 5], y_pred)


# Plotting the tree

library(rattle)
fancyRpartPlot(model = classifier)

