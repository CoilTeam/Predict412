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
trainsize <- nrow(tic.train)
testsize <- nrow(tic.test)
trainsize
testsize
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
colnames(combinedresults)  <- c("Method_Name", "Validation_Set_True_Positives", "TP_Identified", "Baseline", "Lift", "Balanced_Train_Set?")
# kludge
combinedresults <- combinedresults[-1,]
###################################################################################
# balance train data
library(unbalanced)
#balance the dataset
# 'type' parameter:  The argument type can take the following values: "ubOver" (over-sampling), "ubUnder" (undersampling), "ubSMOTE" (SMOTE), "ubOSS" (One Side Selection), "ubCNN" (Condensed Nearest Neighbor), "ubENN" (Edited Nearest Neighbor), "ubNCL" (Neighborhood Cleaning Rule),"ubTomek" (Tomek Link)
ytrainfactor <- as.factor(ytrain)
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
  liftcut <- order[1:350,]
  TP <- sum(liftcut$y)
  Lift  <- TP-Baseline 
  results  <- as.vector(c(methodname, test_truepos, TP, Baseline, Lift, BT))
}
###################################################################################
# set classification thresholds
# this isn't needed anymore, it is a holdover from when we were evaluating performance
# in general terms rather than via the COiL approach
# leaving it here to test some models ad-hoc
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
methodname  <- "neural network"
BT <- "Y"
tic.nnet <- nnet(CARAVAN~ABRAND + APERSAUT + AWAOREG + MGEMOMV + STYPE, data=bal.tic.train, size=4)
predictions  <- predict(tic.nnet, newdata=tic.test)
results1  <- evaluate(methodname, predictions, combinedresults)
combinedresults  <- rbind(combinedresults, results1)
combinedresults
# same performance as glm

# try nathan's model formula:
BT <- "Y"
methodname  <- "neural network"
# try nathan's model formula:
model <- CARAVAN ~ MOPLLAAG+MBERBOER+MAUT1+MINK3045+MINK4575+PWAPART+PWALAND+PPERSAUT+PTRACTOR+PGEZONG+PBRANDnum+PFIETS+AWAPART+AWALAND+APERSAUT+ABESAUT+AVRAAUT+ATRACTOR+ABROM+APERSONG+AGEZONG+ABRAND+APLEZIER+AFIETS+ABYSTAND

bal.tic.train$PBRANDnum <- as.numeric(bal.tic.train$PBRAND)
tic.test$PBRANDnum <- as.numeric(tic.test$PBRAND)

tic.nnet <- nnet(CARAVAN ~ MOPLLAAG+MBERBOER+MAUT1+MINK3045+MINK4575+PWAPART+PWALAND+PPERSAUT+PTRACTOR+PGEZONG+PBRANDnum+PFIETS+AWAPART+AWALAND+APERSAUT+ABESAUT+AVRAAUT+ATRACTOR+ABROM+APERSONG+AGEZONG+ABRAND+APLEZIER+AFIETS+ABYSTAND, data=bal.tic.train, size=7)
predictions  <- predict(tic.nnet, newdata=tic.test)
results1  <- evaluate(methodname, predictions, combinedresults)
combinedresults  <- rbind(combinedresults, results1)
combinedresults
###################################################################################
# naive bayes
library(e1071)
BT <- "Y"
methodname  <- "naive bayes"
tic.nb <- naiveBayes(ticformula, data=tic.train)
tic.nb <- naiveBayes(ticformula, data=bal.tic.train)
tic.nb <- naiveBayes(bal.xtrain, bal.ytrain)
predictions <- predict(tic.nb, newdata=tic.test[,-86], type='raw')[,2]
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

# try nathan's model formula:
model <- CARAVAN ~ MOPLLAAG+MBERBOER+MAUT1+MINK3045+MINK4575+PWAPART+PWALAND+PPERSAUT+PTRACTOR+PGEZONG+PBRANDnum+PFIETS+AWAPART+AWALAND+APERSAUT+ABESAUT+AVRAAUT+ATRACTOR+ABROM+APERSONG+AGEZONG+ABRAND+APLEZIER+AFIETS+ABYSTAND
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

# use T/F predictions for use with learning curves
methodname  <- "naive bayes"
BT <- "N"
#set up matrix to hold results
lcmat <- t(as.matrix(c(1,1)))
colnames(lcmat) <- c("train_err", "test_err")
lcmat <- lcmat[-1,]
# set up vector of train set sizes
trainfraction  <- seq(0.1,1,0.1)
trainlength <- trunc(as.vector(trainsize*trainfraction))
trainlength

#iterate model over increasing sample size
for(i in trainlength){  
  tic.nb <- naiveBayes(ticformula, data=tic.train[1:i,])
    yhat_train <- predict(tic.nb,tic.train[1:i,])
    yhat_test <- predict(tic.nb, newdata=tic.test)
    yhat_train<- ifelse(yhat_train=="insurance", 1, 0)
    yhat_test <- ifelse(yhat_test=="insurance", 1, 0)
    # train error
    traincomb  <- as.data.frame(cbind(ytrain[1:i],yhat_train))
    traincomb  <- within(traincomb, residual <- abs(ytrain[1:i]-yhat_train))
    train_err <- round(sum(traincomb$residual)/nrow(traincomb), digits=4)
  train_err
    # test error
    testcomb  <- as.data.frame(cbind(ytest,yhat_test))
    testcomb  <- within(testcomb, residual <- abs(ytest-yhat_test))
    test_err <- round(sum(testcomb$residual)/nrow(testcomb), digits=4)
    results  <- as.vector(c(train_err, test_err))
    lcmat <- as.data.frame(rbind(lcmat,results)) 
}

write.xlsx2(lcmat,"D:/R Working Directory/Predict412/teamproject/learningcurve.xlsx",col.names=TRUE, row.names=FALSE)

# get the range for the x and y axis
xrange <- range(trainlength)
yrange <- range(c(0,lcmat$test_err))
linetype <- c(1:2)
plotchar <- seq(18,19,1)

#plot the learning curve
plot(xrange,yrange,xlab="Number of training examples",ylab="Error")
lines(trainlength, lcmat$train_err, type="b", lwd=1.5, lty=linetype[1], col=colors[1], pch=plotchar[1]) 
lines(trainlength, lcmat$test_err, type="b", lwd=1.5, lty=linetype[2], col=colors[2], pch=plotchar[2]) 
legend(x=500, y=0.8, c("train", "test"), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Naive Bayes Learning Curve")
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
# Nathan's GLM model, on balanced and unbalanced data
methodname  <- "GLM"
BT <- "N"
model <- CARAVAN ~ MOPLLAAG+MBERBOER+MAUT1+MINK3045+MINK4575+PWAPART+PWALAND+PPERSAUT+PTRACTOR+PGEZONG+PBRANDnum+PFIETS+AWAPART+AWALAND+APERSAUT+ABESAUT+AVRAAUT+ATRACTOR+ABROM+APERSONG+AGEZONG+ABRAND+APLEZIER+AFIETS+ABYSTAND
tic.train$PBRANDnum <- as.numeric(tic.train$PBRAND)
tic.test$PBRANDnum <- as.numeric(tic.test$PBRAND)
tic.glm <- glm(model, data=tic.train, family=binomial(link='logit'))
predictions  <- predict(tic.glm, newdata=tic.test, type='response')
results1  <- evaluate(methodname, predictions, combinedresults)
combinedresults  <- rbind(combinedresults, results1)
combinedresults

methodname  <- "GLM"
BT <- "Y"
model <- CARAVAN ~ MOPLLAAG+MBERBOER+MAUT1+MINK3045+MINK4575+PWAPART+PWALAND+PPERSAUT+PTRACTOR+PGEZONG+PBRANDnum+PFIETS+AWAPART+AWALAND+APERSAUT+ABESAUT+AVRAAUT+ATRACTOR+ABROM+APERSONG+AGEZONG+ABRAND+APLEZIER+AFIETS+ABYSTAND
bal.tic.train$PBRANDnum <- as.numeric(bal.tic.train$PBRAND)
tic.test$PBRANDnum <- as.numeric(tic.test$PBRAND)
tic.glm <- glm(model, data=bal.tic.train, family=binomial(link='logit'))
predictions  <- predict(tic.glm, newdata=tic.test, type='response')
results1  <- evaluate(methodname, predictions, combinedresults)
combinedresults  <- rbind(combinedresults, results1)
combinedresults
###################################################################################
# Chas' interaction approach
# load data and check data integrity
data(ticdata)
nrow(ticdata)
ticdata <- ticdata[1:5822,] # Select training data only
#Separate STYPE
dim(ticdata)
head(as.numeric(ticdata[,"STYPE"]))
str(ticdata[,"STYPE"])
MOSTYPE = as.numeric(ticdata[,"STYPE"])
head(MOSTYPE)
STYPE1 = ifelse(MOSTYPE>30,MOSTYPE,0)
STYPE2 = ifelse(MOSTYPE<=30,ifelse(MOSTYPE>20,MOSTYPE,0),0)
STYPE3 = ifelse(MOSTYPE<=20,ifelse(MOSTYPE>10,MOSTYPE,0),0)
STYPE4 = ifelse(MOSTYPE<=10,ifelse(MOSTYPE>1,MOSTYPE,0),0)
head(data.frame(MOSTYPE, STYPE1, STYPE2, STYPE3, STYPE4), n=50)
ticdata$STYPE1 = factor(STYPE1)
ticdata$STYPE2 = factor(STYPE2)
ticdata$STYPE3 = factor(STYPE3)
ticdata$STYPE4 = factor(STYPE4)
head(ticdata)

### Create interaction Variables based on pre-selected subset 
### (coming from a union of RF, Rpart, sw and Lasso on original variables) 
### Code not included here 
Xint = model.matrix(~(ABRAND
                      +APERSAUT
                      +APLEZIER
                      +AWAOREG
                      +MBERARBG
                      +MFWEKIND
                      +MGEMOMV
                      +MHHUUR
                      +MINKGEM
                      +MKOOPKLA
                      +MOPLHOOG
                      +MOPLLAAG
                      +MOSHOOFD
                      +STYPE1
                      +STYPE4
                      +PBRAND
                      +PPERSAUT
                      +PPLEZIER
                      +PWAPART
                      +STYPE)^2-1,ticdata) 
str(Xint) 
# write.csv(Xint, file="Xint.csv", sep=",", col.names=TRUE) 

# created the output of the full dataset with interaction variables
# write.csv(Xint, file="Xint.csv", sep=",", col.names=TRUE)
### The above outputs the full set of dependent variables 
### and all of their possible combinations of interaction variables

# check outputs are aligned with independent variable
data.frame(ticdata[1:20, c("CARAVAN","ABRAND")],Xint[1:20,"ABRAND"])
all(ticdata[,"ABRAND"] == Xint[,"ABRAND"])
# True
## coerce to df; DO NOT use as.data.frame.model.matrix as this results in one column
class(Xint)
Xint  <-  as.data.frame(Xint)
class(Xint)
dim(Xint) # note the super high dimensionality
# note redundant columns with ticdata; ignore for now, b/c new x frame to be created
head(Xint)
names(Xint)
Xint[1:5,1:10] # first cols
Xint[1:5,(ncol(Xint)-5):ncol(Xint)] # last cols
which( colnames(Xint)=="AWAOREG" ) #last duplicate (b/c simple binomial response)
str(ticdata)
# create Y table to append
which(colnames(ticdata)=="CARAVAN")
Yint   <-  as.data.frame(ticdata[,86])
head(Yint)
colnames(Yint) <- "Yint1"
Yint$CARAVAN  <-  ifelse(Yint$Yint1== "insurance",1,0)
head(Yint)
Yint  <- as.matrix(Yint$CARAVAN)
colnames(Yint) <- "CARAVAN"
ticdata=cbind(Yint,Xint)

class(ticdata)
ticdata[1:5,(ncol(ticdata)-5):ncol(ticdata)]
head(ticdata)
ticdata[1:5,1:5]
dim(ticdata)  # note the super high dimensionality - should be 5822 13160

write.csv(ticdata, file="ticdata_expanded.csv", sep = ",", col.names = TRUE)
###################################################################################
### VARIABLE SELECTION
### Lasso  with interaction variables
ticdata<- read.csv("D:/R Working Directory/Predict412/teamproject/ticdata_expanded.csv")
dim(ticdata)

library(glmnet)
fit = glmnet(Xint, Yint, family="binomial", alpha=1)
plot(fit)


# use cross validation to determine lambda (s):
cvfit = cv.glmnet(Xint, Yint, family = "binomial", type.measure = "class", nfolds=4)
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se

# result of lambda min:
Lasso_Results  <-  data.matrix(coef(cvfit, s = "lambda.min"))

lassoterms  <- as.data.frame(Lasso_Results)
colnames(lassoterms) <- 'coeff'
lassoterms <- within(lassoterms, abs <- abs(coeff))
lassoterms <- lassoterms[-1,]
keepterms <- lassoterms[lassoterms$abs>=0.00001,]
# check min coeff size
arrange(keepterms, abs)
# reduce the data set to include the indicators selected by lasso
keepcols  <- match(rownames(keepterms), colnames(ticdata))
CARAVAN <- ticdata$CARAVAN
tic_reduced <- cbind(CARAVAN, ticdata[,keepcols]) 
dim(tic_reduced)
write.xlsx(tic_reduced, file="tic_reduced2.xlsx", sep = ",", col.names = TRUE)
###################################################################################
# This section repeats the original preparation code - want to retain it
# while editing as needed to test the reduced set
#ticdata<- read.csv("D:/R Working Directory/Predict412/teamproject/ticdata_reduced.csv")
tic <- as.data.frame(tic_reduced)
dim(ticdata)
# change CARAVAN back to char to reuse the rest of this code
tic$CARAVAN <- ifelse(tic$CARAVAN==1, "insurance", "noinsurance")
tic[1:5,1:5]
rows <- nrow(tic)
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
library(sampling)
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
trainsize <- nrow(tic.train)
testsize <- nrow(tic.test)
trainsize
testsize
# Many algorithms won't do well if the data is presented one class then the other in the train set
tic.train   <-  tic.train [sample(nrow(tic.train)),]
dim(tic.train)
tic.train[1:5,1:5]
###################################################################################
# set up new formula for reduced data set
ticvars <- setdiff(colnames(tic.train),list('CARAVAN'))
#ticvars
# ticformula <- as.formula(paste('CARAVAN=="CARAVAN"', paste(ticvars,collapse=' + '),sep=' ~ '))
ticformula <- as.formula(paste('CARAVAN', paste(ticvars,collapse=' + '),sep=' ~ '))
ticformula
###################################################################################
# break out response and predictors for reduced dataset
ytrain <- ifelse(tic.train$CARAVAN=="insurance",1,0)
which(colnames(tic.train)=="CARAVAN")
xtrain <- as.data.frame(tic.train[,-1])
ytest <- ifelse(tic.test$CARAVAN=="insurance",1,0)
xtest <- as.data.frame(tic.test[,-1])
##################################################################################
# balance train data
library(unbalanced)
# 'type' parameter:  The argument type can take the following values: "ubOver" (over-sampling), "ubUnder" (undersampling), "ubSMOTE" (SMOTE), "ubOSS" (One Side Selection), "ubCNN" (Condensed Nearest Neighbor), "ubENN" (Edited Nearest Neighbor), "ubNCL" (Neighborhood Cleaning Rule),"ubTomek" (Tomek Link)
ytrainfactor <- as.factor(ytrain)
balancedData<- ubBalance(as.data.frame(xtrain),ytrainfactor, type="ubOver", seed=123)
bal.tic.train<-cbind(balancedData$X,balancedData$Y)
head(bal.tic.train)
bal.tic.train <- rename(bal.tic.train, c("balancedData$Y" = "CARAVAN"))
bal.xtrain  <- as.data.frame(balancedData$X)
head(bal.xtrain)
bal.ytrain <- bal.tic.train$CARAVAN
###################################################################################
# logistic regression via glm
# change CARAVAN back to numeric for this part
tic.train$CARAVAN <- ifelse(tic.train$CARAVAN=="insurance", 1, 0)

methodname  <- "logistic regression via glm"
BT <- "Y"
# log regression using the 5 predictors identified by an earlier tree model
head(bal.tic.train)
tic.glm <- glm(ticformula, data=bal.tic.train, family=binomial(link='logit'))
predictions  <- predict(tic.glm, newdata=tic.test, type='response')
results1  <- evaluate(methodname, predictions, combinedresults)
combinedresults  <- rbind(combinedresults, results1)
combinedresults

methodname  <- "logistic regression via glm"
BT <- "N"
summary(tic.train$CARAVAN)
tic.glm <- glm(ticformula, data=tic.train, family=binomial(link='logit'))
predictions  <- predict(tic.glm, newdata=tic.test, type='response')
results1  <- evaluate(methodname, predictions, combinedresults)
combinedresults  <- rbind(combinedresults, results1)
combinedresults
###################################################################################
# naive bayes
library(e1071)
BT <- "Y"
methodname  <- "naive bayes"
tic.nb <- naiveBayes(ticformula, data=tic.train)
tic.nb <- naiveBayes(ticformula, data=bal.tic.train)
tic.nb <- naiveBayes(bal.xtrain, bal.ytrain)
predictions <- predict(tic.nb, newdata=tic.test[,-86], type='raw')[,2]
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
