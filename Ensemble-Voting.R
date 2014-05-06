##############################################################################
## PROJECT COIL
## Nathan Susanj
## Ensemble voting to produce better modeling results
## April 29, 2014

## I created a simple voting loop to create new predictions from three different
## models. The ensemble approach had a "best of both" worlds quality to it. It
## didn't have the highest AUC of all models, but it had good Recall and a good
## AUC, which was difficult to find in just one model. We might be able to
## combine some of our better models this way to get a "super-model" to tackle
## the problem.

## I made some attempts at variable transformations, but I think more work needs
## to be done on that front to get better results too.

##############################################################################

# Load COIL Data
library(kernlab)
data(ticdata)

##############################################################################
########################### DATA PREPROCESSING ###############################
##############################################################################

tic <- ticdata[1:5822,] # Select training data only
str(tic)

# Convert ordinal factors to regular factors to be able to use ROSE package
for (n in 6:50) {
  tic[,n] <- as.integer(tic[,n])
  tic[,n] <- as.factor(tic[,n])
}
# Convert P variables to integers
for (n in 51:64) {
  tic[,n] <- as.integer(tic[,n])
}
str(tic)

# Create Product-Contribution interaction variables
tic$IAANHANG <- tic$PAANHANG*tic$AAANHANG
tic$ITRACTOR <- tic$PTRACTOR*tic$ATRACTOR
tic$IWERKT <- tic$PWERKT*tic$AWERKT
tic$IBROM <- tic$PBROM*tic$ABROM
tic$ILEVEN <- tic$PLEVEN*tic$ALEVEN
tic$IPERSONG <- tic$PPERSONG*tic$APERSONG
tic$IGEZONG <- tic$PGEZONG*tic$AGEZONG
tic$IWAOREG <- tic$PWAOREG*tic$AWAOREG
tic$IBRAND <- tic$PBRAND*tic$ABRAND
tic$IZEILPL <- tic$PZEILPL*tic$AZEILPL
tic$IPLEZIER <- tic$PPLEZIER*tic$APLEZIER
tic$IFIETS <- tic$PFIETS*tic$AFIETS
tic$IINBOED <- tic$PINBOED*tic$AINBOED
tic$IBYSTAND <- tic$PBYSTAND*tic$ABYSTAND

# Convert P variables back to factors
for (n in 51:64) {
  tic[,n] <- as.factor(tic[,n])
}

# Create a sub-training set and test set to validate results
set.seed(123)
train=sample(5822,4075)
tic.train <- tic[train,]
tic.test <- tic[-train,]

# Subset using just the demographic and interaction variables
tic2 <- tic[,c(1:50,86:100)]
tic.train2 <- tic2[train,]
tic.test2 <- tic2[-train,]

# Subset using just the purchase power and interaction variables
tic3 <- tic[,c(50,86:100)]
tic.train3 <- tic3[train,]
tic.test3 <- tic3[-train,]

##############################################################################
################################ MODELING ####################################
##############################################################################

# Fit different types of models
library(ROSE)
library(e1071)

# Fit Decision Tree model
tree.fit <- rpart(CARAVAN~.,data=tic.rose6) 
pred.tree <- predict(tree.fit,type="class",tic.test) # save predicted responses
with(tic.test,table(CARAVAN,pred.tree)) # Recall: .6848
with(tic.test,roc.curve(CARAVAN,pred.tree,col=1)) # AUC: 0.638

# Fit Naive Bayes model
nb.fit <- naiveBayes(CARAVAN~.,data=tic.train2)
pred.nb <- predict(nb.fit,tic.test)
with(tic.test2,table(CARAVAN,pred.nb)) # Recall: 0.8152
with(tic.test2,roc.curve(CARAVAN,pred.nb,col=1)) # AUC: 0.574

# Fit Random Forest model on over-sampled data
tic.rose <- ovun.sample(CARAVAN~.,data=tic.train3,method="over",p=0.5,seed=123)$data
rf.fit <- randomForest(CARAVAN~.,data=tic.rose,ntree=501)
pred.rf <- predict(rf.fit,tic.test3)
with(tic.test3,table(CARAVAN,pred.rf)) # Recall: 0.6522
with(tic.test3,roc.curve(CARAVAN,pred.rf,col=1)) # AUC: .658

##############################################################################
########################### ENSEMBLE RESULTS #################################
##############################################################################

# Voting Ensemble: Mode function finds the most frequent prediction from the models
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate the most popular classification from each three models, save predictions in a loop
pred.ens <- NULL
for (n in 1:length(pred.nb2)) {
  pred.ens <- c(pred.ens,as.numeric(Mode(c(pred.nb[n],pred.tree[n],pred.rf[n]
  ))))
}
pred.ens <- as.factor(pred.ens) # Change predictions vector to factor
levels(pred.ens) <- c("no insurance","insurance") # Change factor levels

# Confusion matrix for ensemble model
with(tic.test,table(CARAVAN,pred.ens)) # Recall: 0.7826
with(tic.test,roc.curve(CARAVAN,pred.ens,col=1)) # AUC = .648
