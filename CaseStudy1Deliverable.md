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
#Q2 - Merge Beer and Breweries files by Brew_ID (Assumes Beers.csv file has renamed Brewery_ID to Brew_ID) and prints the first/last 6 observations.
beers <- read.csv("CleanedBeerData.csv", header = TRUE)
breweries <- read.csv("CleanedBreweryData.csv", header = TRUE)
master.file <- Reduce(function(beers, breweries) merge(beers, breweries, by="Brew_ID"), list(beers, breweries))
write.csv(master.file, file = "masterfile.csv", row.names = FALSE)
state.count <- as.data.frame(table(master.file$State))
names(state.count) <- c("State", "Brewery_Count")
state.count
```

```
##    State Brewery_Count
## 1     AK            24
## 2     AL            10
## 3     AR             5
## 4     AZ            47
## 5     CA           181
## 6     CO           259
## 7     CT            27
## 8     DC             8
## 9     DE             2
## 10    FL            58
## 11    GA            16
## 12    HI            26
## 13    IA            30
## 14    ID            28
## 15    IL            89
## 16    IN           138
## 17    KS            23
## 18    KY            21
## 19    LA            19
## 20    MA            79
## 21    MD            20
## 22    ME            27
## 23    MI           162
## 24    MN            54
## 25    MO            42
## 26    MS            11
## 27    MT            40
## 28    NC            56
## 29    ND             3
## 30    NE            23
## 31    NH             8
## 32    NJ             8
## 33    NM            14
## 34    NV            11
## 35    NY            73
## 36    OH            49
## 37    OK            19
## 38    OR           114
## 39    PA           100
## 40    RI            27
## 41    SC            14
## 42    SD             7
## 43    TN             6
## 44    TX           130
## 45    UT            25
## 46    VA            39
## 47    VT            26
## 48    WA            67
## 49    WI            84
## 50    WV             2
## 51    WY            15
```

###First 6, Last 6
Some of the observations are seen below.  The data gathered includes 


```r
head(master.file,6)
```

```
##   Brew_ID        Name.x Beer_ID   ABV IBU
## 1       1  Get Together    2692 0.045  50
## 2       1 Maggie's Leap    2691 0.049  26
## 3       1    Wall's End    2690 0.048  19
## 4       1       Pumpion    2689 0.060  38
## 5       1    Stronghold    2688 0.060  25
## 6       1   Parapet ESB    2687 0.056  47
##                                 Style Ounces             Name.y
## 1                        American IPA     16 NorthGate Brewing 
## 2                  Milk / Sweet Stout     16 NorthGate Brewing 
## 3                   English Brown Ale     16 NorthGate Brewing 
## 4                         Pumpkin Ale     16 NorthGate Brewing 
## 5                     American Porter     16 NorthGate Brewing 
## 6 Extra Special / Strong Bitter (ESB)     16 NorthGate Brewing 
##          City State
## 1 Minneapolis    MN
## 2 Minneapolis    MN
## 3 Minneapolis    MN
## 4 Minneapolis    MN
## 5 Minneapolis    MN
## 6 Minneapolis    MN
```

```r
tail(master.file,6)
```

```
##      Brew_ID                    Name.x Beer_ID   ABV IBU
## 2361     556             Pilsner Ukiah      98 0.055  NA
## 2362     557  Heinnieweisse Weissebier      52 0.049  NA
## 2363     557           Snapperhead IPA      51 0.068  NA
## 2364     557         Moo Thunder Stout      50 0.049  NA
## 2365     557         Porkslap Pale Ale      49 0.043  NA
## 2366     558 Urban Wilderness Pale Ale      30 0.049  NA
##                        Style Ounces                        Name.y
## 2361         German Pilsener     12         Ukiah Brewing Company
## 2362              Hefeweizen     12       Butternuts Beer and Ale
## 2363            American IPA     12       Butternuts Beer and Ale
## 2364      Milk / Sweet Stout     12       Butternuts Beer and Ale
## 2365 American Pale Ale (APA)     12       Butternuts Beer and Ale
## 2366        English Pale Ale     12 Sleeping Lady Brewing Company
##               City State
## 2361         Ukiah    CA
## 2362 Garrattsville    NY
## 2363 Garrattsville    NY
## 2364 Garrattsville    NY
## 2365 Garrattsville    NY
## 2366     Anchorage    AK
```

###Number of NA's
Some of the data was incomplete.  A summary of incomplete information is shown below.


```r
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


###Maximum Alcoholic (ABV) and Most Bitter(IBU) Beer
Based on the information gathered "Name of state" has the highest alcohol by volumne (ABV) and "Name of state" has the highest rating of International Bitterness Units (IBU) compared to the other states.


###ABV Summary
"Insert data about Min, Max, quartiles, etc."


###IBU and ABV relationship
"Insert data and description of relationshipbetween IBU and ABV"


#Summary


