rm(list=ls())
set.seed(123)
library(plyr)
library(xlsx)
library(sampling)
library(pls)
library(MASS)
library(kernlab)
library(ROCR)
library(caret)
setwd("D:/R Working Directory/Predict412/teamproject")
# load data and check data integrity
data(ticdata)
nrow(ticdata)
tic <- ticdata[1:5822,] # Select training data only
head(tic)
rows <- nrow(tic)
compcases  <- sum(complete.cases(tic))
# if this logical test returns false, data is missing.
rows==compcases
# there is no missing data
###################################################################################
summary(tic)
# there are 348 positive responses in the total set
posinst <- nrow(tic[tic$CARAVAN=="insurance",])
# base rate for positive response (CARAVAN=1) is ~ 6%
baserate <- round(posinst/nrow(tic), digits=4)
baserate
# for 70/30 split, the test set should have 104 positive responses, +/- 1
pos_test_obs <- trunc(baserate*nrow(tic)*.3)
pos_test_obs
###################################################################################
# partition into train and test
# stratify first, otherwise could be easy for one set to get no pos responses
# calculate how to stratify to get equal representation in train and test:
# how many positive instances should the train set have?
postrain <- trunc(posinst*.7)
postrain
# how many negative instances should the train set have?
negtrain <- trunc((rows-posinst)*.7)
negtrain
# add a stratum column which will be used to split the set into train and test:
strata <- strata(tic, stratanames=c("CARAVAN"), size=c(negtrain,postrain), method="srswor")
# split into train and test
tic.train <- tic[rownames(tic) %in% strata$ID_unit,]
tic.test <- tic[!rownames(tic) %in% strata$ID_unit,]
# here is how the respective sets look:
table(tic.train$CARAVAN)
table(tic.test$CARAVAN)
# after stratification, the test set is 2 rows longer than the strict 70% calc, and has 105 positive responses rather than the raw calc of 104, which is fine:
nrow(tic.train)
nrow(tic.test)
# Many algorithms won't do well if the data is presented one class then the other in the train set
tic.train   <-  tic.train [sample(nrow(tic.train)),]
###################################################################################
# break out response and predictors for those methods that need them separated
ytrain <- ifelse(tic.train$CARAVAN=="insurance",1,0)
which(colnames(tic.train)=="CARAVAN")
xtrain <- as.data.frame(tic.train[,-86])
ytest <- ifelse(tic.test$CARAVAN=="insurance",1,0)
xtest <- as.data.frame(tic.test[,-86])
# save number of true positive responses in the test set for use in table later
test_truepos <- sum(ytest)
# make a formula for general use across methods
ticvars <- setdiff(colnames(tic.train),list('CARAVAN'))
#ticvars
# ticformula <- as.formula(paste('CARAVAN=="CARAVAN"', paste(ticvars,collapse=' + '),sep=' ~ '))
ticformula <- as.formula(paste('CARAVAN', paste(ticvars,collapse=' + '),sep=' ~ '))
ticformula
###################################################################################
# set up repository to collect results
trainpreds  <- as.data.frame(ytrain)
testpreds  <- as.data.frame(ytest)
colnames(trainpreds) <- "trainy"
colnames(testpreds) <- "testy"
###################################################################################
# set up metrics frame - run once
combinedresults  <- as.data.frame(c(1:6))
combinedresults  <- t(combinedresults)
colnames(combinedresults)  <- c("Method_Name", "Test_Set_True_Positives", "TP_Identified", "Baseline", "Lift", "Balanced_Train_Set?")
# kludge
combinedresults <- combinedresults[-1,]
###################################################################################
# balance train data
library(unbalanced)
#balance the dataset
# 'type' parameter:  The argument type can take the following values: "ubOver" (over-sampling), "ubUnder" (undersampling), "ubSMOTE" (SMOTE), "ubOSS" (One Side Selection), "ubCNN" (Condensed Nearest Neighbor), "ubENN" (Edited Nearest Neighbor), "ubNCL" (Neighborhood Cleaning Rule),"ubTomek" (Tomek Link)
ytrainfactor <- as.factor(ytrain)
ytestfactor <- as.factor(ytrain)
balancedData<- ubBalance(as.data.frame(xtrain),ytrainfactor, type="ubOver")
bal.tic.train<-cbind(balancedData$X,balancedData$Y)
head(bal.tic.train)
bal.tic.train <- rename(bal.tic.train, c("balancedData$Y" = "CARAVAN"))
bal.xtrain  <- as.data.frame(balancedData$X)
head(bal.xtrain)
bal.ytrain <- bal.tic.train$CARAVAN
summary(bal.tic.train)
###################################################################################
# Scoring 
# CoIL asked for the 800 test observations with highest probability of being positive responses
# The final holdout set has 4000 rows, so 800 is the top 20%
# We want to do the same thing in our test set (to identify our best model)
# our test set has 1748 rows
testsetsize <- nrow(tic.test)
testsetsize
# we want to select our top 20%, so this is 350 predictions:
topquintile_test <- round((testsetsize*.2), digits=0)
topquintile_test
# In any random sample of 350 observations from the data, there should be 20 or 21 positive responses:
baserate*topquintile_test
# this is very close to 21, so let's round it up
Baseline <- 21
# the best model is the one that achieves the highest lift over 21 in the top 350
# (350 with highest predicted Pr(yhat=1|X))
###################################################################################
# create function to evaluate results
evaluate  <- function(methodname, predictions, combinedresults){
  lift <- as.data.frame(cbind(ytest, predictions))
  colnames(lift) <- c("y", "yhat")
  order <- lift[order(lift$yhat,decreasing=TRUE),]
  head(order)
  liftcut <- order[1:350,]
  TP <- sum(liftcut$y)
  Lift  <- TP-Baseline 
  results  <- as.vector(c(methodname, test_truepos, TP, Baseline, Lift, BT))
}
###################################################################################
# set classification thresholds
threshold  <- seq(.1, 0.9, 0.1)
###################################################################################
# save final results
#combinedresults <- combinedresults[-1,]
#combinedresults
write.xlsx2(combinedresults,"D:/R Working Directory/Predict412/teamproject/results.xlsx",col.names=TRUE, row.names=FALSE)
###################################################################################
# tree model
library(rpart)
# next two lines pass the method name and whether balanced data was used or not to the final results table:
methodname  <- "tree model"
BT <- "Y"
# run the tree
tic.tree <- rpart(ticformula,data=bal.tic.train)
predictions<- as.data.frame(predict(tic.tree, newdata=tic.test))[,2]
results1  <- evaluate(methodname, predictions, combinedresults)
combinedresults  <- rbind(combinedresults, results1)
combinedresults
# for computational eda:
printcp(tic.tree)
library(maptree)
draw.tree(tic.tree, cex=0.5, nodeinfo=TRUE, col=gray(0:8 / 8))
###################################################################################
# logistic regression via glm
# next two lines pass the method name and whether balanced data was used or not 
# to the final results table:
methodname  <- "logistic regression via glm"
BT <- "Y"
# log regression using the 5 predictors identified by an earlier tree model
tic.glm <- glm(CARAVAN~ABRAND+APERSAUT+AWAOREG+MGEMOMV+STYPE, data=bal.tic.train, family=binomial(link='logit'))
predictions  <- predict(tic.glm, newdata=tic.test, type='response')
results1  <- evaluate(methodname, predictions, combinedresults)
combinedresults  <- rbind(combinedresults, results1)
combinedresults
# so this model beat the tree - identified additional 2 TPs
###################################################################################
# neural network
library(nnet)
BT <- "Y"
methodname  <- "neural network"
tic.nnet <- nnet(CARAVAN~ABRAND + APERSAUT + AWAOREG + MGEMOMV + STYPE, data=bal.tic.train, size=4)
predictions  <- predict(tic.nnet, newdata=tic.test)
results1  <- evaluate(methodname, predictions, combinedresults)
combinedresults  <- rbind(combinedresults, results1)
combinedresults
# same performance as glm
###################################################################################
# naive bayes
library(e1071)
BT <- "Y"
methodname  <- "naive bayes"
tic.nb <- naiveBayes(bal.xtrain, bal.ytrain)
predictions <- predict(tic.nb, newdata=tic.test[,-86], type='raw')[,2]
head(predictions)
results1  <- evaluate(methodname, predictions=predictions, combinedresults)
combinedresults  <- rbind(combinedresults, results1)
combinedresults

# test NB on unbalanced data:
BT <- "N"
methodname  <- "naive bayes"
tic.nb <- naiveBayes(xtrain, ytrain)
predictions <- predict(tic.nb, newdata=tic.test[,-86], type='raw')[,2]
head(predictions)
results1  <- evaluate(methodname, predictions=predictions, combinedresults)
combinedresults  <- rbind(combinedresults, results1)
combinedresults
###################################################################################
# save final results
#combinedresults <- combinedresults[-1,]
#combinedresults
write.xlsx2(combinedresults,"D:/R Working Directory/Predict412/teamproject/results.xlsx",col.names=TRUE, row.names=FALSE)
###################################################################################
# Methods which predict T/F rather than conditional probabilities
# not sure how to get probabilities out for comparison with the other results
###################################################################################
# boosting
methodname  <- "boosting"
library(ada)
BT <- "Y"
tic.ada <- ada(ticformula, data=bal.tic.train, type="discrete", loss="logistic")
predictions <- predict(tic.ada, newdata=tic.test)
table(actual=ytest, predicted=predictions)
###################################################################################
# bagging
methodname  <- "bagging"
BT <- "N"
library(adabag)
# I don't think this is really converging.  Posterior probabilities using the
# balanced data are all NaN.  Definitely doesn't converge on unbalanced train data
tic.bag <- bagging(CARAVAN~., data=bal.tic.train)
yhat<- predict(tic.bag, newdata=tic.test)
yhat$predictions <- ifelse(yhat$yhat=="insurance", 1, 0)
predictions  <- ifelse(yhat=="insurance", 1, 0)
sum(yhat$predictions)
table(predictions)
results1  <- evaluate(methodname, predictions=testpreds$bag, combinedresults)
combinedresults  <- rbind(combinedresults, results1)
###################################################################################
# bayes tree
library(BayesTree)
methodname  <- "bayes tree"
BT <- "N"
tic.bt  <- bart(bal.xtrain, bal.ytrain, tic.test[, -86])
str(tic.bt)
library(slam)
predictions <- as.data.frame(col_means(tic.bt$yhat.test))
head(predictions)
testpredsbt$obs <- c(1:1748)
head(testpredsbt)
colnames(testpredsbt) <- "yhat"
order <- testpredsbt[order(testpredsbt$yhat,decreasing=TRUE),]
head(order)
###################################################################################






