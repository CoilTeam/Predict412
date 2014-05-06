## PROJECT COIL
## Nathan Susanj
## Over-sampling to produce better modeling results
## April 28, 2014

## This modeling attempt primarily uses simple rpart trees, but the trees are fit
## on oversampled data using R's ROSE package. Since the proportion of customers
## that have caravan insurance policies is so small, I thought it might be helful
## to use an oversampling technique to improve the modeling results.

## Using the ROSE package and the ovun.sample() function, I was able to improve
## the AUC (area under the ROC curve) for my decision tree model from the initial
## model that predicted only "no insurance" (AUC=0.500, equivalent of a random 
## guess), to a model that captured 53 of the 92 caravan owners in the test set
## on 443 caravan predictions (AUC=.670). The data set that yielded this result
## is named tic.rose5 in the code below. It oversamples the caravan insurance
## owners so that they make up 45% of the population.

## More trial and error along these lines, especially with more powerful
## algorithms, may be useful for our project goal. I started with some random
## forests implementations toward the end, but will continue to work on this.
## I had a lot of success with support vector machines in the last individual
## programming assignment. I plan to try those using this method as well.

# Load COIL Data
library(kernlab)
data(ticdata)

tic <- ticdata[1:5822,] # Select training data only

# Convert ordinal factors to regular factors to be able to use ROSE package
for (n in 6:64) {
  tic[,n] <- as.integer(tic[,n])
  tic[,n] <- as.factor(tic[,n])
}
str(tic)

# Create a sub-training set and test set to validate results
set.seed(123)
train=sample(5822,4075)
tic.train <- tic[train,]
tic.test <- tic[-train,]

# Evaluate competing tree models with class imbalance correction
library(ROSE)

# Standard tree model
library(rpart)
tree.fit1 <- rpart(CARAVAN~.,data=tic.train)
pred.tree1 <- predict(tree.fit1,type="class",tic.test) # save predicted responses

# Create more "balanced" data set through ROSE package
tic.rose <- ROSE(CARAVAN ~ .,data=tic.train,seed=123)$data
tic.rose2 <- ovun.sample(CARAVAN~.,data=tic.train,method="over",p=0.15,seed=123)$data
tic.rose3 <- ovun.sample(CARAVAN~.,data=tic.train,method="over",p=0.25,seed=123)$data
tic.rose4 <- ovun.sample(CARAVAN~.,data=tic.train,method="over",p=0.35,seed=123)$data
tic.rose5 <- ovun.sample(CARAVAN~.,data=tic.train,method="over",p=0.45,seed=123)$data
tic.rose6 <- ovun.sample(CARAVAN~.,data=tic.train,method="over",p=0.75,seed=123)$data

# Compare class distributions
with(tic.train,table(CARAVAN)) # Severe class imbalance (256 insurance, 3819 no insurance)
with(tic.rose,table(CARAVAN)) # Class balance achieved (2029 insurance, 2046 no insurance)
with(tic.rose2,table(CARAVAN)) # Class balance achieved (659 insurance, 3819 no insurance)
with(tic.rose3,table(CARAVAN)) # Class balance achieved (1252 insurance, 3819 no insurance)
with(tic.rose4,table(CARAVAN)) # Class balance achieved (2042 insurance, 3819 no insurance)
with(tic.rose5,table(CARAVAN)) # Class balance achieved (3105 insurance, 3819 no insurance)
with(tic.rose6,table(CARAVAN)) # Class balance achieved (11490 insurance, 3819 no insurance)

# ROSE adjusted tree models
tree.fit2 <- rpart(CARAVAN~.,data=tic.rose)
pred.tree2 <- predict(tree.fit2,type="class",tic.test) # save predicted responses

tree.fit3 <- rpart(CARAVAN~.,data=tic.rose2)
pred.tree3 <- predict(tree.fit3,type="class",tic.test) # save predicted responses

tree.fit4 <- rpart(CARAVAN~.,data=tic.rose3)
pred.tree4 <- predict(tree.fit4,type="class",tic.test) # save predicted responses

tree.fit5 <- rpart(CARAVAN~.,data=tic.rose4)
pred.tree5 <- predict(tree.fit5,type="class",tic.test) # save predicted responses

tree.fit6 <- rpart(CARAVAN~.,data=tic.rose5)
pred.tree6 <- predict(tree.fit6,type="class",tic.test) # save predicted responses

tree.fit7 <- rpart(CARAVAN~.,data=tic.rose6)
pred.tree7 <- predict(tree.fit7,type="class",tic.test) # save predicted responses

# Test set confusion matrix for each model (92 customers of 1747 have caravan insurance)
with(tic.test,table(CARAVAN,pred.tree1)) # 0 predicted for all observations
with(tic.test,table(CARAVAN,pred.tree2)) # insurance predicted for all but 3 observations
with(tic.test,table(CARAVAN,pred.tree3)) # 77 insurance predicted, 12 correctly
with(tic.test,table(CARAVAN,pred.tree4)) # 133 insurance predicted, 21 correctly
with(tic.test,table(CARAVAN,pred.tree5)) # 374 insurance predicted, 38 correctly
with(tic.test,table(CARAVAN,pred.tree6)) # 443 insurance predicted, 53 correctly
with(tic.test,table(CARAVAN,pred.tree7)) # 741 insurance predicted, 63 correctly

# Evaluate three models via ROC Curve
with(tic.test,roc.curve(CARAVAN,pred.tree1)) # AUC = .500
with(tic.test,roc.curve(CARAVAN,pred.tree2,add.roc=TRUE,col=2)) # AUC = .501
with(tic.test,roc.curve(CARAVAN,pred.tree3,add.roc=TRUE,col=3)) # AUC = .546
with(tic.test,roc.curve(CARAVAN,pred.tree4,add.roc=TRUE,col=4)) # AUC = .580
with(tic.test,roc.curve(CARAVAN,pred.tree5,add.roc=TRUE,col=5)) # AUC = .605
with(tic.test,roc.curve(CARAVAN,pred.tree6,add.roc=TRUE,col=6)) # AUC = .670
with(tic.test,roc.curve(CARAVAN,pred.tree7,add.roc=TRUE,col=7)) # AUC = .638


# Fit a random forest model on the best over-sampled data (tic.rose5)
library(randomForest)
# Random forests cannot handle categorical variables with more than 32 categories
# Delete STYPE variable
tic.rose5.v2 <- tic.rose5
tic.rose5.v2$STYPE <- NULL

# Random forest model #1
rf.fit1 <- randomForest(CARAVAN~.,data=tic.rose5.v2)
pred.rf1 <- predict(rf.fit1,type="class",tic.test) # save predicted responses

# Test set confusion matrix
with(tic.test,table(CARAVAN,pred.rf1)) # 100 predicted, 8 correctly
with(tic.test,roc.curve(CARAVAN,pred.rf1,add.roc=TRUE,col=8)) # AUC = .516
