# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('SampleSuperstore (1).csv')
dataset[,1:9] = lapply(dataset[,1:9], factor)
class(dataset$Ship.Mode)
dataset = dataset[,-c(3,4,6)]
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


 regressor = lm(formula = Profit ~ .,
                data = training_set)
 summary(regressor)
 
 # Predicting the results
# Profit vs Category
 library(dplyr)
 training_set=training_set %>% group_by(Category) %>% mutate(ncategory=n()/nrow(training_set))
 ggplot(training_set,aes(y=training_set$Profit,x=training_set$Category, fill=training_set$Category))+geom_violin(aes(weight=ncategory ))+ggtitle('profit vs categories')+xlab('categories') +ylab('Profit')
 # profit vs sub.category
 ggplot(training_set,aes(y=Profit,x=Sub.Category))+geom_jitter(aes(col= Sub.Category))+ggtitle('profit vs Sub_categories')+xlab('Sub_categories') +ylab('Profit')
 
 #profit vs quantity
 ggplot(training_set,aes(y= Profit,x=Quantity))+geom_jitter(aes(col=Quantity)) +ggtitle('profit vs quantity')+xlab('Quantity') +ylab('Profit')
 
 #profil vs discount 
 ggplot(training_set,aes(y= Profit,x=Discount))+geom_jitter(aes(col=Discount))+ggtitle('profit vs Discount')+xlab('Discount') +ylab('Profit')
 
 #Profit vs sales
 ggplot(training_set,aes(y= Profit,x=Sales))+geom_jitter(aes(col=Sales))+ggtitle('profit vs Sales')+xlab('Sales') +ylab('Profit')
 
 
