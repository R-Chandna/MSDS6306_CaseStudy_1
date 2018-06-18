---
title: "Case Study 1"
author: "Rajat Chandna, Andy Ho, An Nguyen, Jodi Pafford, Tori Wheelis"
date: "June 6, 2018"
output:
  html_document:
    keep_md: yes
---



```r
# read in Beers data set with correct character formatting.
Beers <- read.csv("Beers.csv", header=TRUE, fileEncoding = 'UTF-8') 


#remove all duplicates, only if name, abv, ibu, brewery id, style and volume are the same.
UniqueBeers <- Beers[-which(duplicated(Beers$Name) & duplicated(Beers$ABV) & duplicated(Beers$IBU) & duplicated(Beers$Brewery_id) & duplicated(Beers$Style) & duplicated(Beers$Ounces)),]


#Keeping An's code for UniqueBeers but adding Jodi's as UniqueBeer2 in case An's version is correct. The code below keeps everything is not a duplicate in Name, ABV, IBU, Brewery_ID, Style and Ounces.
UniqueBeers2 <- Beers[!duplicated(Beers[c(1,3,4,5,6,7)]),]
```

####Tidying Breweries Data

```r
#Tidying Breweries.csv Section. Completed by Rajat and Andy
#First inspect the State column.  We listed out the States and got the count for each.  If observation > 50, we investigated further by examining all the States with a count of 1 (DC, ND, SD, WV).  In this case, there was 51 States and confirmed the 51st was DC which is acceptable.  All abbreviations in State column is valid.
breweries <- read.csv("Breweries.csv", header = TRUE)
str(breweries)
```

```
## 'data.frame':	558 obs. of  4 variables:
##  $ Brew_ID: int  1 2 3 4 5 6 7 8 9 10 ...
##  $ Name   : Factor w/ 551 levels "10 Barrel Brewing Company",..: 355 12 266 319 201 136 227 477 59 491 ...
##  $ City   : Factor w/ 384 levels "Abingdon","Abita Springs",..: 228 200 122 299 300 62 91 48 152 136 ...
##  $ State  : Factor w/ 51 levels " AK"," AL"," AR",..: 24 18 20 5 5 41 6 23 23 23 ...
```

```r
summary(breweries)
```

```
##     Brew_ID                           Name           City    
##  Min.   :  1.0   Blackrocks Brewery     :  2   Portland: 17  
##  1st Qu.:140.2   Blue Mountain Brewery  :  2   Boulder :  9  
##  Median :279.5   Lucette Brewing Company:  2   Chicago :  9  
##  Mean   :279.5   Oskar Blues Brewery    :  2   Seattle :  9  
##  3rd Qu.:418.8   Otter Creek Brewing    :  2   Austin  :  8  
##  Max.   :558.0   Sly Fox Brewing Company:  2   Denver  :  8  
##                  (Other)                :546   (Other) :498  
##      State    
##   CO    : 47  
##   CA    : 39  
##   MI    : 32  
##   OR    : 29  
##   TX    : 28  
##   PA    : 25  
##  (Other):358
```

```r
table(breweries$State)
```

```
## 
##  AK  AL  AR  AZ  CA  CO  CT  DC  DE  FL  GA  HI  IA  ID  IL  IN  KS  KY 
##   7   3   2  11  39  47   8   1   2  15   7   4   5   5  18  22   3   4 
##  LA  MA  MD  ME  MI  MN  MO  MS  MT  NC  ND  NE  NH  NJ  NM  NV  NY  OH 
##   5  23   7   9  32  12   9   2   9  19   1   5   3   3   4   2  16  15 
##  OK  OR  PA  RI  SC  SD  TN  TX  UT  VA  VT  WA  WI  WV  WY 
##   6  29  25   5   4   1   3  28   4  16  10  23  20   1   4
```


```r
#In this section, looking to clean up the "City" column.  Still looking for a more elequent code but for now creating subsets for each letter and visually identifying misspellings, abbreviations, and/or punctuations.  Rajat to research a function that creates a subset of cities by subsetting cities with the same 3 letter string.
unique(breweries[grep("[.]", breweries$City), "City"])
```

```
## [1] St. Paul     Mt. Airy     St. John's   Mt. Pleasant
## 384 Levels: Abingdon Abita Springs Ada Afton Airway Heights ... York
```

```r
unique(breweries[grep("^[Mm].*", breweries$City), "City"])
```

```
##  [1] Minneapolis        Marquette          Martinsville      
##  [4] Michigan City      Mishawaka          Murphysboro       
##  [7] Manhattan          Mt. Airy           Madison           
## [10] Morganton          Milwaukee          Monroe            
## [13] Middleton          Middleburg Heights Memphis           
## [16] Mount Pleasant     Middlebury         Montauk           
## [19] Mill Valley        Meridian           Meadville         
## [22] Missoula           Monument           Midvale           
## [25] Mustang            Midwest City       Menominee         
## [28] Manheim            Mt. Pleasant       Miami             
## [31] Moab               Macon              Medford           
## [34] Menominie          Minnetonka         Marietta          
## [37] Mooresville        Marlborough        Myrtle Beach      
## [40] Mammoth Lakes     
## 384 Levels: Abingdon Abita Springs Ada Afton Airway Heights ... York
```

```r
unique(breweries[grep("^[Ss].*", breweries$City), "City"])
```

```
##  [1] San Diego           San Francisco       South Lyon         
##  [4] Seven Points        Stevens Point       St. Paul           
##  [7] Saint Louis         San Antonio         St Petersburg      
## [10] Sheridan            St Mary's           St Paul            
## [13] Springdale          Sisters             Salt Lake City     
## [16] Shelburne           Spring Lake         Springfield        
## [19] Seattle             Spearfish           Stillwater         
## [22] Stamford            Southampton         Santa Cruz         
## [25] Somerset Center     South Austin        Shreveport         
## [28] South Deerfield     South Burlington    St. John's         
## [31] Santee              South Bend          San Luis Obispo    
## [34] Shelbyville         Santa Fe            Sacramento         
## [37] Savannah            Slippery Rock       Stratford          
## [40] Soldotna            South San Francisco Spirit Lake        
## [43] Sheffield           Silverton           Stevensville       
## [46] Smithton           
## 384 Levels: Abingdon Abita Springs Ada Afton Airway Heights ... York
```


```r
#With State and City cleaned up, we check for duplicates breweries by matching the name, each's city, than state.  Still in process.
```
