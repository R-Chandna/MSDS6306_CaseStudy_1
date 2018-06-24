---
title: "CaseStudy1Deliverable"
author: "Rajat Chandna, Andy Ho, An Nguyen, Jodi Pafford, and Tori Wheelis"
date: "June 17, 2018"
output: 
  html_document:
    keep_md: yes
---

#Introduction

The purpose of this study is to help "Company Name" assess the market size and competition on where the next "Company Name" brewery should be built and what type of beer it should brew. The data used in this study was derived from "Author of Data" and includes 2410 US craft beers from 558 US breweries. (see Codebook for specific details)

#Guiding Questions

###Breweries per state
The 558 breweries are spread among "# of States".  The most breweries are located in "state".  


```r
#Q2 - Merge Beer and Breweries files by Brew_ID and count of Breweries by State
beers <- read.csv("CleanedBeerData.csv", header = TRUE)
breweries <- read.csv("CleanedBreweryData.csv", header = TRUE)
master.file <- Reduce(function(beers, breweries) merge(beers, breweries, by="Brew_ID"), list(beers, breweries))
write.csv(master.file, file = "masterfile.csv", row.names = FALSE)
state.count <- as.data.frame(table(master.file$State))
names(state.count) <- c("State", "Brewery_Count")
statesSort <- state.count[order(state.count$Brewery_Count),]
statesSort
```

```
##    State Brewery_Count
## 9     DE             2
## 50    WV             2
## 29    ND             3
## 3     AR             5
## 43    TN             6
## 42    SD             7
## 8     DC             8
## 31    NH             8
## 32    NJ             8
## 2     AL            10
## 26    MS            11
## 34    NV            11
## 33    NM            14
## 41    SC            14
## 51    WY            15
## 11    GA            16
## 19    LA            19
## 37    OK            19
## 21    MD            20
## 18    KY            21
## 17    KS            23
## 30    NE            23
## 1     AK            24
## 45    UT            25
## 12    HI            26
## 47    VT            26
## 7     CT            27
## 22    ME            27
## 40    RI            27
## 14    ID            28
## 13    IA            30
## 46    VA            39
## 27    MT            40
## 25    MO            42
## 4     AZ            47
## 36    OH            49
## 24    MN            54
## 28    NC            56
## 10    FL            58
## 48    WA            67
## 35    NY            73
## 20    MA            79
## 49    WI            84
## 15    IL            89
## 39    PA           100
## 38    OR           114
## 44    TX           130
## 16    IN           138
## 23    MI           162
## 5     CA           181
## 6     CO           259
```

```r
#Add Graphic for Presentation Purposes
library(ggplot2)
StateCount <- ggplot(statesSort, aes(reorder(State, -Brewery_Count), Brewery_Count, color=State)) + geom_bar(stat = "Identity", width = .85) + labs(x = "State") + theme(legend.position = "none") + coord_flip()
StateCount
```

![](CaseStudy1Deliverable_files/figure-html/Q1-1.png)<!-- -->

###First 6, Last 6
Some of the observations are seen below.  The data gathered includes 


```rq2
#Q2 - Merge Beer and Breweries files by Brew_ID and prints the first/last 6 observations.
beers <- read.csv("Beers.csv", header = TRUE)
names(breweries) <- c("Brewery_id", "Name", "City", "State")
master.file <- Reduce(function(beers, breweries) merge(beers, breweries, by="Brewery_id"), list(beers, breweries))
names(master.file) <- c("Brewery_id", "Beer_name", "Beer_id", "ABV", "IBU", "Style", "Ounces", "Brewery_name", "City", "State")
head(master.file,6)
tail(master.file,6)
write.csv(master.file, file = "masterfile.csv", row.names = FALSE)
```

###Number of NA's
Some of the data was incomplete.  A summary of incomplete information is shown below.


```r
#Q3 - Summing the NA's for each column
colSums(is.na(master.file))
```

```
## Brew_ID  Name.x Beer_ID     ABV     IBU   Style  Ounces  Name.y    City 
##       0       0       0      62     997       0       0       0       0 
##   State 
##       0
```

###Median Alcohol Content & International Bitterness Unti (IBU) for each state
The median amount of alcohol content and IBUs in each state is listed below. "Add Info about scatterplot"

```rq4
medianIBUABV<-aggregate.data.frame(master.file[, 4:5], list(master.file$State), median, na.rm=TRUE)
states<-rep(medianIBUABV$Group.1,2)
values<-c(medianIBUABV$ABV, medianIBUABV$IBU)
type <-c(rep("ABV", 51), rep("IBU", 51))
data<-data.frame(states, values)

ggplot(data, aes(states, values)) +
  geom_bar(stat = "identity", aes(fill = type), position = "dodge") +
  xlab("States") +
  ggtitle("Median State ABV & IBU") +
  theme_bw()

# This works but need to work on the scale of the ABV
```

###Maximum Alcoholic (ABV) and Most Bitter(IBU) Beer
Based on the information gathered "Name of state" has the highest alcohol by volumne (ABV) and "Name of state" has the highest rating of International Bitterness Units (IBU) compared to the other states.

```r
MaxABV <- aggregate(ABV~State, data=master.file, max)
MaxABV
```

```
##    State   ABV
## 1     AK 0.068
## 2     AL 0.093
## 3     AR 0.061
## 4     AZ 0.095
## 5     CA 0.099
## 6     CO 0.128
## 7     CT 0.090
## 8     DC 0.092
## 9     DE 0.055
## 10    FL 0.082
## 11    GA 0.072
## 12    HI 0.083
## 13    IA 0.095
## 14    ID 0.099
## 15    IL 0.096
## 16    IN 0.120
## 17    KS 0.085
## 18    KY 0.125
## 19    LA 0.088
## 20    MA 0.099
## 21    MD 0.085
## 22    ME 0.099
## 23    MI 0.099
## 24    MN 0.099
## 25    MO 0.080
## 26    MS 0.080
## 27    MT 0.075
## 28    NC 0.099
## 29    ND 0.067
## 30    NE 0.096
## 31    NH 0.065
## 32    NJ 0.099
## 33    NM 0.080
## 34    NV 0.099
## 35    NY 0.100
## 36    OH 0.099
## 37    OK 0.085
## 38    OR 0.088
## 39    PA 0.099
## 40    RI 0.086
## 41    SC 0.097
## 42    SD 0.069
## 43    TN 0.062
## 44    TX 0.099
## 45    UT 0.090
## 46    VA 0.088
## 47    VT 0.096
## 48    WA 0.084
## 49    WI 0.099
## 50    WV 0.067
## 51    WY 0.072
```

```r
MaxABV[order(MaxABV$ABV),]
```

```
##    State   ABV
## 9     DE 0.055
## 3     AR 0.061
## 43    TN 0.062
## 31    NH 0.065
## 29    ND 0.067
## 50    WV 0.067
## 1     AK 0.068
## 42    SD 0.069
## 11    GA 0.072
## 51    WY 0.072
## 27    MT 0.075
## 25    MO 0.080
## 26    MS 0.080
## 33    NM 0.080
## 10    FL 0.082
## 12    HI 0.083
## 48    WA 0.084
## 17    KS 0.085
## 21    MD 0.085
## 37    OK 0.085
## 40    RI 0.086
## 19    LA 0.088
## 38    OR 0.088
## 46    VA 0.088
## 7     CT 0.090
## 45    UT 0.090
## 8     DC 0.092
## 2     AL 0.093
## 4     AZ 0.095
## 13    IA 0.095
## 15    IL 0.096
## 30    NE 0.096
## 47    VT 0.096
## 41    SC 0.097
## 5     CA 0.099
## 14    ID 0.099
## 20    MA 0.099
## 22    ME 0.099
## 23    MI 0.099
## 24    MN 0.099
## 28    NC 0.099
## 32    NJ 0.099
## 34    NV 0.099
## 36    OH 0.099
## 39    PA 0.099
## 44    TX 0.099
## 49    WI 0.099
## 35    NY 0.100
## 16    IN 0.120
## 18    KY 0.125
## 6     CO 0.128
```

```r
MaxIBU <- aggregate(IBU~State, data=master.file, max)
MaxIBU
```

```
##    State IBU
## 1     AK  71
## 2     AL 103
## 3     AR  39
## 4     AZ  99
## 5     CA 115
## 6     CO 104
## 7     CT  85
## 8     DC 115
## 9     DE  52
## 10    FL  82
## 11    GA  65
## 12    HI  75
## 13    IA  99
## 14    ID 100
## 15    IL 100
## 16    IN 115
## 17    KS 110
## 18    KY  80
## 19    LA  60
## 20    MA 130
## 21    MD  90
## 22    ME  70
## 23    MI 115
## 24    MN 120
## 25    MO  89
## 26    MS  80
## 27    MT  80
## 28    NC  98
## 29    ND  70
## 30    NE  65
## 31    NH  82
## 32    NJ 100
## 33    NM 100
## 34    NV  90
## 35    NY 111
## 36    OH 126
## 37    OK 100
## 38    OR 138
## 39    PA 113
## 40    RI  75
## 41    SC  65
## 42    TN  61
## 43    TX 118
## 44    UT  83
## 45    VA 135
## 46    VT 120
## 47    WA  83
## 48    WI  80
## 49    WV  71
## 50    WY  75
```

```r
MaxIBU[order(MaxIBU$IBU),]
```

```
##    State IBU
## 3     AR  39
## 9     DE  52
## 19    LA  60
## 42    TN  61
## 11    GA  65
## 30    NE  65
## 41    SC  65
## 22    ME  70
## 29    ND  70
## 1     AK  71
## 49    WV  71
## 12    HI  75
## 40    RI  75
## 50    WY  75
## 18    KY  80
## 26    MS  80
## 27    MT  80
## 48    WI  80
## 10    FL  82
## 31    NH  82
## 44    UT  83
## 47    WA  83
## 7     CT  85
## 25    MO  89
## 21    MD  90
## 34    NV  90
## 28    NC  98
## 4     AZ  99
## 13    IA  99
## 14    ID 100
## 15    IL 100
## 32    NJ 100
## 33    NM 100
## 37    OK 100
## 2     AL 103
## 6     CO 104
## 17    KS 110
## 35    NY 111
## 39    PA 113
## 5     CA 115
## 8     DC 115
## 16    IN 115
## 23    MI 115
## 43    TX 118
## 24    MN 120
## 46    VT 120
## 36    OH 126
## 20    MA 130
## 45    VA 135
## 38    OR 138
```

###Maximum Alcoholic (ABV) and Most Bitter(IBU) Beer
Based on the information gathered, Colorado has the beer with the highest alcohol by volumne (ABV) and Oregon has the beer with the highest rating of International Bitterness Units (IBU) compared to the other states.

```rq5

```

###ABV Summary
"Insert data about Min, Max, quartiles, etc."


```rq6
summary(BeersandBreweries$ABV)
```

###IBU and ABV relationship
"Insert data and description of relationship between IBU and ABV"

```rq7
plot(ABV, IBU, main="Relationship Between Beer Bitternes and its Alcoholic Content",
   xlab="ABV: Alcohol By Volume", ylab="IBU: International Bitterness Units", pch=19) 

ggplot(master.file, aes(x=ABV, y=IBU)) + geom_point()
```

#Summary


