**Setup**

```r
library(kernlab)
# library(car)
library(ggplot2)
library(corrgram)
```

```
## Loading required package: seriation
```

```r
library(vcd)
```

```
## Loading required package: grid
```

```r
library(rpart)
library(randomForest)
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
library(e1071)
library(nnet)
library(MASS)
library(bootstrap)
library(DAAG)
```

```
## Loading required package: lattice
## 
## Attaching package: 'lattice'
## 
## The following object is masked from 'package:seriation':
## 
##     panel.lines
## 
## 
## Attaching package: 'DAAG'
## 
## The following object is masked from 'package:MASS':
## 
##     hills
```


### 2.1.1 Import Dataset

```r
data(ticdata)
# raw <- read.csv('../ticdata2000.csv')
data <- ticdata[1:5822, ]
```


#### Association Plot (Original Categ. Vars.)
_Works with categorical variables_
_Paused for the time being to increase speed_

```r
for (i in 1:64) {
    t <- table(data[, c(i, 86)])
    assoc(t, shade = T)
}
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-31.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-32.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-33.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-34.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-35.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-36.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-37.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-38.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-39.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-310.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-311.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-312.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-313.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-314.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-315.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-316.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-317.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-318.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-319.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-320.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-321.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-322.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-323.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-324.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-325.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-326.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-327.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-328.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-329.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-330.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-331.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-332.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-333.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-334.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-335.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-336.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-337.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-338.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-339.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-340.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-341.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-342.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-343.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-344.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-345.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-346.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-347.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-348.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-349.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-350.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-351.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-352.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-353.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-354.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-355.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-356.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-357.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-358.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-359.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-360.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-361.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-362.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-363.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-364.png) 


#### Bar Plots (Original Categ. Vars.)

```r
for (i in 1:64) {
    p <- ggplot(data, aes(x = data[, i], fill = CARAVAN)) + geom_bar() + labs(x = names(data)[i])
    print(p)
}
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-43.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-44.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-45.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-46.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-47.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-48.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-49.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-410.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-411.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-412.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-413.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-414.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-415.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-416.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-417.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-418.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-419.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-420.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-421.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-422.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-423.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-424.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-425.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-426.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-427.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-428.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-429.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-430.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-431.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-432.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-433.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-434.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-435.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-436.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-437.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-438.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-439.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-440.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-441.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-442.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-443.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-444.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-445.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-446.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-447.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-448.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-449.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-450.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-451.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-452.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-453.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-454.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-455.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-456.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-457.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-458.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-459.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-460.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-461.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-462.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-463.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-464.png) 


#### Box/whisker Plot (Original Num. Vars.)

```r
for (i in 65:85) {
    formula.string <- paste(names(data)[i], "CARAVAN", sep = "~")
    f <- as.formula(formula.string)
    boxplot(f, data, main = names(data)[i])
}
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-51.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-52.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-53.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-54.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-55.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-56.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-57.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-58.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-59.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-510.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-511.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-512.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-513.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-514.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-515.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-516.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-517.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-518.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-519.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-520.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-521.png) 


#### Association Plot (Converted Num. Vars.)
_Works with categorical variables_
_Paused for the time being to increase speed_

```r
dc <- as.data.frame(lapply(data, as.factor))  # transform all to categorical

for (i in 65:85) {
    t <- table(dc[, c(i, 86)])
    assoc(t, shade = T)
}
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-61.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-62.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-63.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-64.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-65.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-66.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-67.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-68.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-69.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-610.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-611.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-612.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-613.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-614.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-615.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-616.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-617.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-618.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-619.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-620.png) ![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-621.png) 


#### Bar Plots (Converted Num. Vars.)

```r
dc <- as.data.frame(lapply(data, as.factor))  # transform all to categorical

for (i in 65:85) {
    p <- ggplot(dc, aes(x = dc[, i], fill = CARAVAN)) + geom_bar() + labs(x = names(dc)[i])
    print(p)
}
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-71.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-72.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-73.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-74.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-75.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-76.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-77.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-78.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-79.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-710.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-711.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-712.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-713.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-714.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-715.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-716.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-717.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-718.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-719.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-720.png) ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-721.png) 


