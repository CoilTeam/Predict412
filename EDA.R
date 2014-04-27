# Load COIL Data
library(kernlab)
data(ticdata)

str(ticdata) # Look at structure
tictrain <- ticdata[1:5822,] # Select training data only
# Save a new version of the CARAVAN variable in (0,1) format
tictrain$CARAVAN_NUM <- ifelse(tictrain$CARAVAN=='noinsurance',0,1)

# Save variable names in vector
variables <- colnames(tictrain)
# Initialize EDA data set
EDA <- data.frame(
  category=levels(tictrain[,1]),
  variable=variables[1],
  n=tapply(tictrain$CARAVAN_NUM, tictrain[,1], length),
  percentage=round(tapply(tictrain$CARAVAN_NUM, tictrain[,1], mean)*100,2),
  sd=tapply(tictrain$CARAVAN_NUM, tictrain[,1], sd)
)

# Loop the rest of the variables through the EDA
for (n in 2:85){
  EDA <- rbind(EDA,data.frame(
    category=levels(as.factor(tictrain[,n])),
    variable=variables[n],
    n=tapply(tictrain$CARAVAN_NUM, tictrain[,n], length),
    percentage=round(tapply(tictrain$CARAVAN_NUM, tictrain[,n], mean)*100,2),
    sd=tapply(tictrain$CARAVAN_NUM, tictrain[,n], sd)
  ))
}

# Examine the table
View(EDA)

############### EXPLANATION ####################
## The above code creates a data frame that
## has several rows, one for each level of
## each predictor variable, accompanied by
## the percentage of cases within that 
## level that have caravan insurance.
#########################################################################
## Example:
## Category               Variable  n    percentage sd (standard deviation)
## Average Family         MOSHOOFD  886  6.66       0.2494539
## Career Loners          MOSHOOFD  52   0.00       0.0000000
## Conservative families  MOSHOOFD  667  6.30       0.2430886
## Cruising Seniors       MOSHOOFD  205  1.95       0.1386552
## Driven Growers         MOSHOOFD  502  13.15      0.3382552
## Family with grown ups  MOSHOOFD  1563 5.69       0.2318055
## Farmers                MOSHOOFD  276  1.81       0.1336130
## Living well            MOSHOOFD  569  2.64       0.1603505
## Retired and Religeous  MOSHOOFD  550  3.64       0.1873637
## Successful hedonists   MOSHOOFD  552  8.70       0.2820269
#########################################################################
## In the example above, you can see that the "Driven Growers"
## in the MOSHOOFD variable have a higher than average
## percentage of caravan insurance holders (13.15%) 
## compared to other types of customers. This information
## may be useful in the modeling phase.
## Information like this is available for each variable
## by running the above code.
