1. Introduction
============

2. Process
=======
2.1 Data Preparation
----------------
### 2.1.1 Import Dataset

```r
raw <- read.csv("../ticdata2000.csv")
data <- raw  # make a backup just in case
```


### 2.1.2 Modify Datatypes

```r
categorical_names <- names(data)[grep("^m", names(data))]
categorical_names <- c("caravan", categorical_names)
data[, categorical_names] <- lapply(data[, categorical_names], as.factor)
```


2.2 Data Exploration
----------------
### 2.2.1 Summary EDA

```r
head(data)
```

```
##   mostype maanthui mgemomv mgemleef moshoofd mgodrk mgodpr mgodov mgodge
## 1      33        1       3        2        8      0      5      1      3
## 2      37        1       2        2        8      1      4      1      4
## 3      37        1       2        2        8      0      4      2      4
## 4       9        1       3        3        3      2      3      2      4
## 5      40        1       4        2       10      1      4      1      4
## 6      23        1       2        1        5      0      5      0      5
##   mrelge mrelsa mrelov mfalleen mfgekind mfwekind moplhoog moplmidd
## 1      7      0      2        1        2        6        1        2
## 2      6      2      2        0        4        5        0        5
## 3      3      2      4        4        4        2        0        5
## 4      5      2      2        2        3        4        3        4
## 5      7      1      2        2        4        4        5        4
## 6      0      6      3        3        5        2        0        5
##   mopllaag mberhoog mberzelf mberboer mbermidd mberarbg mberarbo mska
## 1        7        1        0        1        2        5        2    1
## 2        4        0        0        0        5        0        4    0
## 3        4        0        0        0        7        0        2    0
## 4        2        4        0        0        3        1        2    3
## 5        0        0        5        4        0        0        0    9
## 6        4        2        0        0        4        2        2    2
##   mskb1 mskb2 mskc mskd mhhuur mhkoop maut1 maut2 maut0 mzfonds mzpart
## 1     1     2    6    1      1      8     8     0     1       8      1
## 2     2     3    5    0      2      7     7     1     2       6      3
## 3     5     0    4    0      7      2     7     0     2       9      0
## 4     2     1    4    0      5      4     9     0     0       7      2
## 5     0     0    0    0      4      5     6     2     1       5      4
## 6     2     2    4    2      9      0     5     3     3       9      0
##   minkm30 mink3045 mink4575 mink7512 mink123m minkgem mkoopkla pwapart
## 1       0        4        5        0        0       4        3       0
## 2       2        0        5        2        0       5        4       2
## 3       4        5        0        0        0       3        4       2
## 4       1        5        3        0        0       4        4       0
## 5       0        0        9        0        0       6        3       0
## 6       5        2        3        0        0       3        3       0
##   pwabedr pwaland ppersaut pbesaut pmotsco pvraaut paanhang ptractor
## 1       0       0        6       0       0       0        0        0
## 2       0       0        0       0       0       0        0        0
## 3       0       0        6       0       0       0        0        0
## 4       0       0        6       0       0       0        0        0
## 5       0       0        0       0       0       0        0        0
## 6       0       0        6       0       0       0        0        0
##   pwerkt pbrom pleven ppersong pgezong pwaoreg pbrand pzeilpl pplezier
## 1      0     0      0        0       0       0      5       0        0
## 2      0     0      0        0       0       0      2       0        0
## 3      0     0      0        0       0       0      2       0        0
## 4      0     0      0        0       0       0      2       0        0
## 5      0     0      0        0       0       0      6       0        0
## 6      0     0      0        0       0       0      0       0        0
##   pfiets pinboed pbystand awapart awabedr awaland apersaut abesaut amotsco
## 1      0       0        0       0       0       0        1       0       0
## 2      0       0        0       2       0       0        0       0       0
## 3      0       0        0       1       0       0        1       0       0
## 4      0       0        0       0       0       0        1       0       0
## 5      0       0        0       0       0       0        0       0       0
## 6      0       0        0       0       0       0        1       0       0
##   avraaut aaanhang atractor awerkt abrom aleven apersong agezong awaoreg
## 1       0        0        0      0     0      0        0       0       0
## 2       0        0        0      0     0      0        0       0       0
## 3       0        0        0      0     0      0        0       0       0
## 4       0        0        0      0     0      0        0       0       0
## 5       0        0        0      0     0      0        0       0       0
## 6       0        0        0      0     0      0        0       0       0
##   abrand azeilpl aplezier afiets ainboed abystand caravan
## 1      1       0        0      0       0        0       0
## 2      1       0        0      0       0        0       0
## 3      1       0        0      0       0        0       0
## 4      1       0        0      0       0        0       0
## 5      1       0        0      0       0        0       0
## 6      0       0        0      0       0        0       0
```

```r
str(data)
```

```
## 'data.frame':	5822 obs. of  86 variables:
##  $ mostype : Factor w/ 40 levels "1","2","3","4",..: 32 36 36 9 39 22 38 32 32 11 ...
##  $ maanthui: Factor w/ 9 levels "1","2","3","4",..: 1 1 1 1 1 1 2 1 1 2 ...
##  $ mgemomv : Factor w/ 5 levels "1","2","3","4",..: 3 2 2 3 4 2 3 2 2 3 ...
##  $ mgemleef: Factor w/ 6 levels "1","2","3","4",..: 2 2 2 3 2 1 2 3 4 3 ...
##  $ moshoofd: Factor w/ 10 levels "1","2","3","4",..: 8 8 8 3 10 5 9 8 8 3 ...
##  $ mgodrk  : Factor w/ 10 levels "0","1","2","3",..: 1 2 1 3 2 1 3 1 1 4 ...
##  $ mgodpr  : Factor w/ 10 levels "0","1","2","3",..: 6 5 5 4 5 6 3 8 2 6 ...
##  $ mgodov  : Factor w/ 6 levels "0","1","2","3",..: 2 2 3 3 2 1 1 1 4 1 ...
##  $ mgodge  : Factor w/ 10 levels "0","1","2","3",..: 4 5 5 5 5 6 6 3 7 3 ...
##  $ mrelge  : Factor w/ 10 levels "0","1","2","3",..: 8 7 4 6 8 1 8 8 7 8 ...
##  $ mrelsa  : Factor w/ 8 levels "0","1","2","3",..: 1 3 3 3 2 7 3 3 1 1 ...
##  $ mrelov  : Factor w/ 10 levels "0","1","2","3",..: 3 3 5 3 3 4 1 1 4 3 ...
##  $ mfalleen: Factor w/ 10 levels "0","1","2","3",..: 2 1 5 3 3 4 1 1 4 3 ...
##  $ mfgekind: Factor w/ 10 levels "0","1","2","3",..: 3 5 5 4 5 6 4 6 4 3 ...
##  $ mfwekind: Factor w/ 10 levels "0","1","2","3",..: 7 6 3 5 5 3 7 5 4 7 ...
##  $ moplhoog: Factor w/ 10 levels "0","1","2","3",..: 2 1 1 4 6 1 1 1 1 1 ...
##  $ moplmidd: Factor w/ 10 levels "0","1","2","3",..: 3 6 6 5 5 6 5 4 2 5 ...
##  $ mopllaag: Factor w/ 10 levels "0","1","2","3",..: 8 5 5 3 1 5 6 7 9 6 ...
##  $ mberhoog: Factor w/ 10 levels "0","1","2","3",..: 2 1 1 5 1 3 1 3 2 3 ...
##  $ mberzelf: Factor w/ 6 levels "0","1","2","3",..: 1 1 1 1 6 1 1 1 2 1 ...
##  $ mberboer: Factor w/ 10 levels "0","1","2","3",..: 2 1 1 1 5 1 1 1 1 1 ...
##  $ mbermidd: Factor w/ 10 levels "0","1","2","3",..: 3 6 8 4 1 5 5 3 2 4 ...
##  $ mberarbg: Factor w/ 10 levels "0","1","2","3",..: 6 1 1 2 1 3 2 6 9 4 ...
##  $ mberarbo: Factor w/ 10 levels "0","1","2","3",..: 3 5 3 3 1 3 6 3 2 4 ...
##  $ mska    : Factor w/ 10 levels "0","1","2","3",..: 2 1 1 4 10 3 1 3 2 2 ...
##  $ mskb1   : Factor w/ 10 levels "0","1","2","3",..: 2 3 6 3 1 3 2 2 2 3 ...
##  $ mskb2   : Factor w/ 10 levels "0","1","2","3",..: 3 4 1 2 1 3 5 3 1 2 ...
##  $ mskc    : Factor w/ 10 levels "0","1","2","3",..: 7 6 5 5 1 5 6 6 9 5 ...
##  $ mskd    : Factor w/ 9 levels "0","1","2","3",..: 2 1 1 1 1 3 1 3 2 3 ...
##  $ mhhuur  : Factor w/ 10 levels "0","1","2","3",..: 2 3 8 6 5 10 7 1 10 1 ...
##  $ mhkoop  : Factor w/ 10 levels "0","1","2","3",..: 9 8 3 5 6 1 4 10 1 10 ...
##  $ maut1   : Factor w/ 10 levels "0","1","2","3",..: 9 8 8 10 7 6 9 5 6 7 ...
##  $ maut2   : Factor w/ 8 levels "0","1","2","3",..: 1 2 1 1 3 4 1 5 3 2 ...
##  $ maut0   : Factor w/ 10 levels "0","1","2","3",..: 2 3 3 1 2 4 2 3 4 3 ...
##  $ mzfonds : Factor w/ 10 levels "0","1","2","3",..: 9 7 10 8 6 10 10 7 8 7 ...
##  $ mzpart  : Factor w/ 10 levels "0","1","2","3",..: 2 4 1 3 5 1 1 4 3 4 ...
##  $ minkm30 : Factor w/ 10 levels "0","1","2","3",..: 1 3 5 2 1 6 5 3 8 3 ...
##  $ mink3045: Factor w/ 10 levels "0","1","2","3",..: 5 1 6 6 1 3 4 6 3 4 ...
##  $ mink4575: Factor w/ 10 levels "0","1","2","3",..: 6 6 1 4 10 4 4 4 2 4 ...
##  $ mink7512: Factor w/ 10 levels "0","1","2","3",..: 1 3 1 1 1 1 1 1 1 2 ...
##  $ mink123m: Factor w/ 8 levels "0","1","2","3",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ minkgem : Factor w/ 10 levels "0","1","2","3",..: 5 6 4 5 7 4 4 4 3 5 ...
##  $ mkoopkla: Factor w/ 8 levels "1","2","3","4",..: 3 4 4 4 3 3 5 3 3 7 ...
##  $ pwapart : int  0 2 2 0 0 0 0 0 0 2 ...
##  $ pwabedr : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ pwaland : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ppersaut: int  6 0 6 6 0 6 6 0 5 0 ...
##  $ pbesaut : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ pmotsco : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ pvraaut : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ paanhang: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ptractor: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ pwerkt  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ pbrom   : int  0 0 0 0 0 0 0 3 0 0 ...
##  $ pleven  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ppersong: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ pgezong : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ pwaoreg : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ pbrand  : int  5 2 2 2 6 0 0 0 0 3 ...
##  $ pzeilpl : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ pplezier: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ pfiets  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ pinboed : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ pbystand: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ awapart : int  0 2 1 0 0 0 0 0 0 1 ...
##  $ awabedr : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ awaland : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ apersaut: int  1 0 1 1 0 1 1 0 1 0 ...
##  $ abesaut : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ amotsco : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ avraaut : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ aaanhang: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ atractor: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ awerkt  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ abrom   : int  0 0 0 0 0 0 0 1 0 0 ...
##  $ aleven  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ apersong: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ agezong : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ awaoreg : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ abrand  : int  1 1 1 1 1 0 0 0 0 1 ...
##  $ azeilpl : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ aplezier: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ afiets  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ ainboed : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ abystand: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ caravan : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
summary(data)
```

```
##     mostype        maanthui    mgemomv  mgemleef    moshoofd   
##  33     : 810   1      :5267   1: 284   1:  74   8      :1563  
##  8      : 339   2      : 505   2:2131   2:1452   3      : 886  
##  38     : 339   3      :  39   3:2646   3:3000   9      : 667  
##  39     : 328   7      :   5   4: 693   4:1073   5      : 569  
##  9      : 278   4      :   2   5:  68   5: 193   1      : 552  
##  23     : 251   5      :   1            6:  30   7      : 550  
##  (Other):3477   (Other):   3                     (Other):1035  
##      mgodrk         mgodpr     mgodov       mgodge         mrelge    
##  0      :3228   4      :1607   0:2003   3      :1453   7      :1683  
##  1      :1599   5      :1501   1:2014   4      :1334   6      :1172  
##  2      : 733   6      : 714   2:1388   2      :1055   5      : 946  
##  3      : 152   3      : 590   3: 257   5      : 963   9      : 794  
##  4      :  66   7      : 564   4: 132   0      : 456   8      : 361  
##  5      :  18   2      : 396   5:  28   1      : 230   4      : 324  
##  (Other):  26   (Other): 450            (Other): 331   (Other): 542  
##      mrelsa         mrelov        mfalleen       mfgekind   
##  0      :2448   2      :1756   0      :1757   3      :1498  
##  1      :2030   0      :1173   2      :1247   4      :1455  
##  2      :1075   3      :1152   1      : 951   2      :1060  
##  3      : 159   4      : 648   3      : 848   5      : 606  
##  4      :  78   1      : 539   4      : 519   1      : 372  
##  5      :  18   5      : 266   5      : 259   0      : 371  
##  (Other):  14   (Other): 288   (Other): 241   (Other): 460  
##     mfwekind       moplhoog       moplmidd       mopllaag   
##  4      :1137   0      :2147   4      :1426   5      :1009  
##  5      :1106   1      :1322   3      :1330   6      : 856  
##  3      : 973   2      :1144   2      : 937   4      : 851  
##  6      : 783   3      : 547   5      : 738   3      : 680  
##  2      : 635   4      : 326   0      : 423   2      : 667  
##  7      : 351   5      : 187   1      : 383   7      : 640  
##  (Other): 837   (Other): 149   (Other): 585   (Other):1119  
##     mberhoog    mberzelf    mberboer       mbermidd       mberarbg   
##  0      :1524   0:4171   0      :4176   2      :1491   2      :1382  
##  2      :1364   1:1202   1      : 854   3      :1394   0      :1167  
##  1      :1245   2: 348   2      : 487   4      : 953   3      :1167  
##  3      : 756   3:  37   3      : 143   0      : 667   1      : 921  
##  4      : 397   4:  12   4      :  77   5      : 431   4      : 604  
##  5      : 249   5:  52   5      :  59   1      : 403   5      : 310  
##  (Other): 287            (Other):  26   (Other): 483   (Other): 271  
##     mberarbo         mska          mskb1          mskb2     
##  2      :1439   0      :1738   2      :1783   2      :1676  
##  3      :1109   1      :1569   1      :1480   3      :1175  
##  1      : 980   2      :1198   0      :1353   0      : 990  
##  0      : 968   3      : 685   3      : 775   1      : 861  
##  4      : 772   4      : 261   4      : 298   4      : 652  
##  5      : 331   5      : 127   5      :  78   5      : 357  
##  (Other): 223   (Other): 244   (Other):  55   (Other): 111  
##       mskc           mskd          mhhuur         mhkoop    
##  5      :1168   0      :2607   0      : 949   9      : 949  
##  4      :1159   1      :1563   9      : 760   0      : 760  
##  3      :1090   2      : 852   2      : 717   7      : 724  
##  2      : 870   3      : 441   3      : 593   6      : 604  
##  6      : 487   4      : 223   8      : 532   1      : 530  
##  0      : 364   5      : 100   5      : 519   5      : 520  
##  (Other): 684   (Other):  36   (Other):1752   (Other):1735  
##      maut1          maut2          maut0         mzfonds    
##  6      :1663   0      :1854   2      :1625   7      :1511  
##  7      :1413   2      :1748   0      :1450   5      : 974  
##  5      :1210   1      :1468   3      :1066   6      : 875  
##  9      : 505   3      : 385   1      : 776   9      : 852  
##  4      : 448   4      : 301   4      : 587   8      : 699  
##  8      : 261   5      :  56   5      : 174   4      : 357  
##  (Other): 322   (Other):  10   (Other): 144   (Other): 554  
##      mzpart        minkm30        mink3045       mink4575   
##  2      :1511   0      :1304   4      :1356   3      :1215  
##  4      : 992   2      :1094   3      :1147   2      :1165  
##  0      : 852   3      :1079   5      : 931   4      :1034  
##  3      : 849   1      : 630   2      : 919   0      : 891  
##  1      : 699   4      : 599   0      : 465   1      : 657  
##  5      : 364   5      : 568   6      : 406   5      : 498  
##  (Other): 555   (Other): 548   (Other): 598   (Other): 362  
##     mink7512       mink123m       minkgem        mkoopkla   
##  0      :3246   0      :4900   3      :1932   3      :1524  
##  1      :1359   1      : 763   4      :1854   4      : 902  
##  2      : 736   2      :  96   5      : 733   6      : 901  
##  3      : 246   3      :  36   2      : 651   1      : 587  
##  4      : 147   4      :  24   6      : 355   5      : 583  
##  5      :  71   5      :   1   7      : 131   7      : 474  
##  (Other):  17   (Other):   2   (Other): 166   (Other): 851  
##     pwapart         pwabedr        pwaland         ppersaut   
##  Min.   :0.000   Min.   :0.00   Min.   :0.000   Min.   :0.00  
##  1st Qu.:0.000   1st Qu.:0.00   1st Qu.:0.000   1st Qu.:0.00  
##  Median :0.000   Median :0.00   Median :0.000   Median :5.00  
##  Mean   :0.771   Mean   :0.04   Mean   :0.072   Mean   :2.97  
##  3rd Qu.:2.000   3rd Qu.:0.00   3rd Qu.:0.000   3rd Qu.:6.00  
##  Max.   :3.000   Max.   :6.00   Max.   :4.000   Max.   :8.00  
##                                                               
##     pbesaut         pmotsco         pvraaut         paanhang    
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.048   Mean   :0.175   Mean   :0.009   Mean   :0.021  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :7.000   Max.   :7.000   Max.   :9.000   Max.   :5.000  
##                                                                 
##     ptractor         pwerkt          pbrom           pleven     
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.093   Mean   :0.013   Mean   :0.215   Mean   :0.195  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :6.000   Max.   :6.000   Max.   :6.000   Max.   :9.000  
##                                                                 
##     ppersong        pgezong          pwaoreg          pbrand    
##  Min.   :0.000   Min.   :0.0000   Min.   :0.000   Min.   :0.00  
##  1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.00  
##  Median :0.000   Median :0.0000   Median :0.000   Median :2.00  
##  Mean   :0.014   Mean   :0.0153   Mean   :0.024   Mean   :1.83  
##  3rd Qu.:0.000   3rd Qu.:0.0000   3rd Qu.:0.000   3rd Qu.:4.00  
##  Max.   :6.000   Max.   :3.0000   Max.   :7.000   Max.   :8.00  
##                                                                 
##     pzeilpl         pplezier         pfiets          pinboed     
##  Min.   :0e+00   Min.   :0.000   Min.   :0.0000   Min.   :0.000  
##  1st Qu.:0e+00   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.000  
##  Median :0e+00   Median :0.000   Median :0.0000   Median :0.000  
##  Mean   :9e-04   Mean   :0.019   Mean   :0.0252   Mean   :0.016  
##  3rd Qu.:0e+00   3rd Qu.:0.000   3rd Qu.:0.0000   3rd Qu.:0.000  
##  Max.   :3e+00   Max.   :6.000   Max.   :1.0000   Max.   :6.000  
##                                                                  
##     pbystand        awapart         awabedr         awaland      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.0000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.0000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.0000  
##  Mean   :0.048   Mean   :0.403   Mean   :0.015   Mean   :0.0206  
##  3rd Qu.:0.000   3rd Qu.:1.000   3rd Qu.:0.000   3rd Qu.:0.0000  
##  Max.   :5.000   Max.   :2.000   Max.   :5.000   Max.   :1.0000  
##                                                                  
##     apersaut        abesaut        amotsco         avraaut      
##  Min.   :0.000   Min.   :0.00   Min.   :0.000   Min.   :0.0000  
##  1st Qu.:0.000   1st Qu.:0.00   1st Qu.:0.000   1st Qu.:0.0000  
##  Median :1.000   Median :0.00   Median :0.000   Median :0.0000  
##  Mean   :0.562   Mean   :0.01   Mean   :0.041   Mean   :0.0022  
##  3rd Qu.:1.000   3rd Qu.:0.00   3rd Qu.:0.000   3rd Qu.:0.0000  
##  Max.   :7.000   Max.   :4.00   Max.   :8.000   Max.   :3.0000  
##                                                                 
##     aaanhang         atractor         awerkt          abrom       
##  Min.   :0.0000   Min.   :0.000   Min.   :0.000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.000   Median :0.000   Median :0.0000  
##  Mean   :0.0125   Mean   :0.034   Mean   :0.006   Mean   :0.0704  
##  3rd Qu.:0.0000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.0000  
##  Max.   :3.0000   Max.   :4.000   Max.   :6.000   Max.   :2.0000  
##                                                                   
##      aleven         apersong         agezong          awaoreg      
##  Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.000   Median :0.0000   Median :0.0000   Median :0.0000  
##  Mean   :0.077   Mean   :0.0053   Mean   :0.0065   Mean   :0.0046  
##  3rd Qu.:0.000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
##  Max.   :8.000   Max.   :1.0000   Max.   :1.0000   Max.   :2.0000  
##                                                                    
##      abrand        azeilpl         aplezier         afiets      
##  Min.   :0.00   Min.   :0e+00   Min.   :0.000   Min.   :0.0000  
##  1st Qu.:0.00   1st Qu.:0e+00   1st Qu.:0.000   1st Qu.:0.0000  
##  Median :1.00   Median :0e+00   Median :0.000   Median :0.0000  
##  Mean   :0.57   Mean   :5e-04   Mean   :0.006   Mean   :0.0318  
##  3rd Qu.:1.00   3rd Qu.:0e+00   3rd Qu.:0.000   3rd Qu.:0.0000  
##  Max.   :7.00   Max.   :1e+00   Max.   :2.000   Max.   :3.0000  
##                                                                 
##     ainboed          abystand      caravan 
##  Min.   :0.0000   Min.   :0.0000   0:5474  
##  1st Qu.:0.0000   1st Qu.:0.0000   1: 348  
##  Median :0.0000   Median :0.0000           
##  Mean   :0.0079   Mean   :0.0143           
##  3rd Qu.:0.0000   3rd Qu.:0.0000           
##  Max.   :2.0000   Max.   :2.0000           
## 
```


### 2.2.2 Univariate EDA

2.3 Data Selection
------------------

2.4 Modeling
------------
### Logistic Regression

```r
y <- names(data)[86]
x <- paste(names(data)[44:85], collapse = "+")
f <- as.formula(paste(y, x, sep = "~"))
print(f)
```

```
## caravan ~ pwapart + pwabedr + pwaland + ppersaut + pbesaut + 
##     pmotsco + pvraaut + paanhang + ptractor + pwerkt + pbrom + 
##     pleven + ppersong + pgezong + pwaoreg + pbrand + pzeilpl + 
##     pplezier + pfiets + pinboed + pbystand + awapart + awabedr + 
##     awaland + apersaut + abesaut + amotsco + avraaut + aaanhang + 
##     atractor + awerkt + abrom + aleven + apersong + agezong + 
##     awaoreg + abrand + azeilpl + aplezier + afiets + ainboed + 
##     abystand
```

```r
lrm <- glm(f, data, family = binomial)
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```


### Decision Tree

```r
require(rpart)
```

```
## Loading required package: rpart
```


### 
