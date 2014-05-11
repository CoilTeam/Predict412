## Below is my best GLM model so far. AUC: 0.7915 (key insight is most of the demographic variables
## actually hurt predictive performance. I only left the ones in that helped after going through
## each variable one by one and evaluating test AUC.

# GLM Model
testROC.glm <- function(model) {
  glm <- glm(model,data=tic.train,family="binomial")
  prob <- abs(predict(glm,newdata=tic.test,type="response")-1)
  plotROC(tic.test$CARAVAN,prob)
}

testROC.glm(model <- CARAVAN ~ MOPLLAAG+MBERBOER+MAUT1+MINK3045+MINK4575+PWAPART+PWALAND+PPERSAUT+PTRACTOR+PGEZONG+PBRANDnum+PFIETS+AWAPART+AWALAND+APERSAUT+ABESAUT+AVRAAUT+ATRACTOR+ABROM+APERSONG+AGEZONG+ABRAND+APLEZIER+AFIETS+ABYSTAND)
# AUC: 0.7915 = best GLM model I could find
model <- CARAVAN ~ MOPLLAAG+MBERBOER+MAUT1+MINK3045+MINK4575+PWAPART+PWALAND+PPERSAUT+PTRACTOR+PGEZONG+PBRANDnum+PFIETS+AWAPART+AWALAND+APERSAUT+ABESAUT+AVRAAUT+ATRACTOR+ABROM+APERSONG+AGEZONG+ABRAND+APLEZIER+AFIETS+ABYSTAND
glm <- glm(model,data=tic.train,family="binomial")
prob <- predict(glm,newdata=tic.test,type="response")
library(Hmisc)
deciles <- cut2(prob,m=as.integer(length(prob)/10))
table(deciles,tic.test$CARAVAN)
pred <- ifelse(prob>0.2,"insurance","noinsurance")
table(tic.test$CARAVAN,pred)

# The variables ending in "num" are versions of the original that I converted to numeric
