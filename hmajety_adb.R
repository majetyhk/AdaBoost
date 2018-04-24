######
# Adaboost Classifier
# Student Name: Hari Krishna Majety
# Student Unity ID: hmajety
######

# Do not clear your workspace

require(rpart) # for decision stump
require(caret)
require(mlbench)
# set seed to ensure reproducibility
set.seed(100)

# calculate the alpha value using epsilon
# params:
# Input: 
# epsilon: value from calculate_epsilon (or error, line 7 in algorithm 5.7 from Textbook)
# output: alpha value (single value) (from Line 12 in algorithm 5.7 from Textbook)
###
calculate_alpha <- function(epsilon){
  alpha <- 0.5*(log((1-epsilon)/epsilon))
  return(alpha)
}

# calculate the epsilon value  
# input:
# weights: weights generated at the end of the previous iteration
# y_true: actual labels (ground truth)
# y_pred: predicted labels (from your newly generated decision stump)
# n_elements: number of elements in y_true or y_pred
# output:
# just the epsilon or error value (line 7 in algorithm 5.7 from Textbook)
###
calculate_epsilon <- function(weights, y_true, y_pred, n_elements){
  epsilon <- sum(weights*ifelse(y_true != y_pred,1,0))/n_elements
  return(epsilon)
}


# Calculate the weights using equation 5.69 from the textbook 
# Input:
# old_weights: weights from previous iteration
# alpha: current alpha value from Line 12 in algorithm 5.7 in the textbook
# y_true: actual class labels
# y_pred: predicted class labels
# n_elements: number of values in y_true or y_pred
# Output:
# a vector of size n_elements containing updated weights
###
calculate_weights <- function(old_weights, alpha, y_true, y_pred, n_elements){
    correspondingAplhas<-ifelse(y_true == y_pred,-alpha,alpha)
    new_weights <- old_weights*exp(correspondingAplhas)
    new_weights<-new_weights/sum(new_weights)
    return(new_weights)
}

# implement myadaboost - simple adaboost classification
# use the 'rpart' method from 'rpart' package to create a decision stump 
# Think about what parameters you need to set in the rpart method so that it generates only a decision stump, not a decision tree
# Input: 
# train: training dataset (attributes + class label)
# k: number of iterations of adaboost
# n_elements: number of elements in 'train'
# Output:
# a vector of predicted values for 'train' after all the iterations of adaboost are completed
###
myadaboost <- function(train, k, n_elements){
  weights<-rep(1/n_elements,n_elements)
  finalPredictions<- rep(0,n_elements)
  for (iter in 1:k) {
    dataSample<-train[sort(sample(n_elements,size = n_elements,replace = TRUE,prob = weights)),]
    fit<-rpart(Label ~ .,data=dataSample,parms = list(split = "information"),control=rpart.control(maxdepth = 1))
    predictionProbabilities<-predict(fit,train[,names(train)!="Label"])
    predictions<- ifelse(predictionProbabilities>=0.5,1,-1)
    #prediction<-predictionProbabilities
    epsilon<-calculate_epsilon(weights,train$Label, predictions, n_elements)
    if(epsilon>0.5){
      weights<-rep(1/n_elements,n_elements)
      iter<-iter-1
      next
    }
    alpha<-calculate_alpha(epsilon)
    weights<-calculate_weights(weights,alpha, train$Label, predictions, n_elements)
    finalPredictions<-finalPredictions + (alpha*predictions)
  }
  finalPredictions<-ifelse(finalPredictions>0,1,-1)
  return(finalPredictions)
}


# Code has already been provided here to preprocess the data and then call the adaboost function
# Implement the functions marked with ### before this line
data("Ionosphere")
Ionosphere <- Ionosphere[,-c(1,2)]
# lets convert the class labels into format we are familiar with in class
# -1 for bad, 1 for good (create a column named 'Label' which will serve as class variable)
Ionosphere$Label[Ionosphere$Class == "good"] = 1
Ionosphere$Label[Ionosphere$Class == "bad"] = -1
# remove unnecessary columns
Ionosphere <- Ionosphere[,-(ncol(Ionosphere)-1)]
# class variable
cl <- Ionosphere$Label
# train and predict on training data using adaboost
predictions <- myadaboost(Ionosphere, 5, nrow(Ionosphere))
# generate confusion matrix
print(table(cl, predictions))
