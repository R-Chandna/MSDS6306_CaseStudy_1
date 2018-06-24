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

#Tidy Data Process
Data for this project came from two different sources (Beers and Brewery).  Both data sets had extensive errors and duplications that were correct or eliminated prior to beginning the research.

#####Beers File

```r
#Tidying of Beers:
#read in Beers data set with correct character formatting.
Beers <- read.csv("Beers.csv", header=TRUE, fileEncoding = 'UTF-8')
```


```r
#Remove duplicate beers if ALL columns but the Beer ID are the same. 
UniqueBeers <- Beers[!duplicated(Beers[c(1,3,4,5,6,7)]),]
```


```r
#Group beers by Brewery and Size
if(FALSE){
  byBrewery <- split(UniqueBeers, list(UniqueBeers$Ounces, UniqueBeers$Brewery_id))

# Match First 3 Chars
for(i in 1:length(byBrewery)){
    uniTempVec <- unique(byBrewery[[i]]$Name)
    tempListVec <- character()
    for(i in 1:length(uniTempVec)){
      subTobeSearched <- substr(uniTempVec[i], 1,3)
      subTobeSearched <- paste0("^",subTobeSearched)
      tempVec <- unique(grep(subTobeSearched, uniTempVec, ignore.case = T, value = T))
      if(length(tempVec) > 1){
        tempListVec <- c(tempListVec, paste(" ", tempVec, collapse = " "))
      }
    }

    unitempListVec <- unique(tempListVec)
    for(i in 1:length(unitempListVec)){
      temp2Vec <- as.vector(unlist(strsplit(trimws(unitempListVec[i], "l"), "   ")))
      if(length(temp2Vec) > 1){
        for(i in 1:length(temp2Vec)){
          cat(temp2Vec[i], file = "./First3matches.csv", append = T)
          if(i != length(temp2Vec)){
            cat(", ", file = "./First3matches.csv", append = T)
          }  
        }
        cat("\n", file = "./First3matches.csv", append = T)
      }
    }
  }

# Match Last 3 chars
  for(i in 1:length(byBrewery)){
    uniTempVec <- as.character(unique(byBrewery[[i]]$Name))
    tempListVec <- character()
    for(i in 1:length(uniTempVec)){
      subTobeSearched <- substr(uniTempVec[i], nchar(uniTempVec[i]) - 3, nchar(uniTempVec[i]))
      subTobeSearched <- paste0(subTobeSearched, "$")
      tempVec <- unique(grep(subTobeSearched, uniTempVec, ignore.case = T, value = T))
      if(length(tempVec) > 1){
        tempListVec <- c(tempListVec, paste(" ", tempVec, collapse = " "))
      }
    }

    unitempListVec <- unique(tempListVec)
    for(i in 1:length(unitempListVec)){
      temp2Vec <- as.vector(unlist(strsplit(trimws(unitempListVec[i], "l"), "   ")))
      if(length(temp2Vec) > 1){
        for(i in 1:length(temp2Vec)){
          cat(temp2Vec[i], file = "./Last3matches.csv", append = T)
          if(i != length(temp2Vec)){
            cat(", ", file = "./Last3matches.csv", append = T)
          }  
        }
        cat("\n", file = "./Last3matches.csv", append = T)
      }
    }
  }
}
```


```r
#Removed beers with identical ABU, IBU, Brewery_id, Ounces but slight differences in name
UniqueBeers <- subset(UniqueBeers, Name != "Ranger IPA (Current)")
UniqueBeers <- subset(UniqueBeers, Name != "Shift (1)")
UniqueBeers <- subset(UniqueBeers, Name != "Point Special (Current)")
UniqueBeers <- subset(UniqueBeers, Name != "Cherry Ale (1)")
UniqueBeers <- subset(UniqueBeers, Name != "Pleasure Town")
UniqueBeers <- subset(UniqueBeers, Name != "Farmer Ted's Cream Ale")
UniqueBeers <- subset(UniqueBeers, Name != "Dry Cider")
UniqueBeers <- subset(UniqueBeers, Name != "Abbey's Single (2015- )")
UniqueBeers <- subset(UniqueBeers, Name != "Triomphe")
UniqueBeers <- subset(UniqueBeers, Name != "Bender")
UniqueBeers <- subset(UniqueBeers, Name != "Hipster Ale (Westbrook Brewing)")
#rename Hipster Ale (Two Roads Brewing) to Hipster Ale, Two Roads Brewing does not brew Hipster Ale
levels(UniqueBeers$Name)[levels(UniqueBeers$Name) == "Hipster Ale (Two Roads Brewing)"] = "Hipster Ale"
```

```r
#Add code to change Brewery_ID column to match the Brews file and create CSV output of clean data.
colnames(UniqueBeers)[colnames(UniqueBeers)=="Brewery_id"] <- "Brew_ID"

#Write Clean File
write.csv(UniqueBeers, file="CleanedBeerData.csv", row.names = F)
```
#####Brewery

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
#With State and City cleaned up, we check for duplicates breweries by matching the name, each's city, than state. 
BrDF <- read.csv("Breweries.csv", header = T, stringsAsFactors = F)

grep("[[:punct:]]",BrDF$City, value = T)
```

```
## [1] "St. Paul"      "Mt. Airy"      "Wilkes-Barre"  "St Mary's"    
## [5] "St. John's"    "Fuquay-Varina" "Mt. Pleasant"  "O'Fallon"
```

```r
grep("[[:punct:][:blank:]]+",BrDF$City, value = T)
```

```
##   [1] "San Diego"           "San Francisco"       "Grand Rapids"       
##   [4] "Grand Rapids"        "Comstock Park"       "South Lyon"         
##   [7] "Grand Rapids"        "Michigan City"       "Seven Points"       
##  [10] "Kansas City"         "Stevens Point"       "San Diego"          
##  [13] "San Francisco"       "Dripping Springs"    "East Fairfield"     
##  [16] "Paso Robles"         "St. Paul"            "Saint Louis"        
##  [19] "Brooklyn Center"     "San Antonio"         "St Petersburg"      
##  [22] "Mt. Airy"            "San Francisco"       "Fort Collins"       
##  [25] "Traverse City"       "East Windsor"        "Wilkes-Barre"       
##  [28] "San Diego"           "Lone Tree"           "North Woodstock"    
##  [31] "St Mary's"           "Stevens Point"       "St Paul"            
##  [34] "Cold Spring"         "Baton Rouge"         "Salt Lake City"     
##  [37] "Fort Worth"          "Traverse City"       "West Chester"       
##  [40] "Garden City"         "White Salmon"        "New Orleans"        
##  [43] "Fort Worth"          "Oklahoma City"       "Spring Lake"        
##  [46] "Paw Paw"             "Oklahoma City"       "San Antonio"        
##  [49] "San Diego"           "Farmers Branch"      "Cherry Hill"        
##  [52] "Atlantic Highlands"  "Middleburg Heights"  "Lake Geneva"        
##  [55] "Lake Havasu City"    "Los Angeles"         "College Station"    
##  [58] "Plant City"          "Santa Cruz"          "Somerset Center"    
##  [61] "South Austin"        "Mount Pleasant"      "Bridgewater Corners"
##  [64] "South Deerfield"     "Mill Valley"         "Ridgefield Park"    
##  [67] "Port Clinton"        "San Diego"           "Idaho Springs"      
##  [70] "Salt Lake City"      "South Burlington"    "Virginia Beach"     
##  [73] "Garden City"         "San Francisco"       "Black Mountain"     
##  [76] "Saint Louis"         "Diamond Springs"     "St. John's"         
##  [79] "Boynton Beach"       "South Bend"          "Green Bay"          
##  [82] "San Luis Obispo"     "Fuquay-Varina"       "Lake Barrington"    
##  [85] "San Francisco"       "Buena Vista"         "Midwest City"       
##  [88] "Mt. Pleasant"        "Airway Heights"      "Santa Fe"           
##  [91] "Ann Arbor"           "Eau Claire"          "San Diego"          
##  [94] "Stevens Point"       "Saint Louis"         "Slippery Rock"      
##  [97] "Santa Cruz"          "Traverse City"       "Royal Oak"          
## [100] "Fort Collins"        "O'Fallon"            "Vadnais Heights"    
## [103] "Half Moon Bay"       "San Diego"           "South San Francisco"
## [106] "Jacksonville Beach"  "Tampa Bay"           "San Antonio"        
## [109] "Spirit Lake"         "Gig Harbor"          "Des Moines"         
## [112] "New York"            "Green Bay"           "Fort Wayne"         
## [115] "Lake Havasu City"    "Colorado Springs"    "San Diego"          
## [118] "Myrtle Beach"        "Santa Cruz"          "New York"           
## [121] "Eau Claire"          "Abita Springs"       "Mammoth Lakes"      
## [124] "Battle Creek"        "North Conway"
```

```r
grep("^..[[:punct:][:blank:]]+",BrDF$City, value = T)
```

```
## [1] "St. Paul"      "St Petersburg" "Mt. Airy"      "St Mary's"    
## [5] "St Paul"       "St. John's"    "Mt. Pleasant"
```

```r
BrDF$City <- gsub("^St[[:punct:][:blank:]]+","Saint ",BrDF$City)
BrDF$City <- gsub("^Mt[[:punct:][:blank:]]+","Mount ",BrDF$City)

uniTempVec <- unique(BrDF$City)
tempListVec <- character()
for(i in 1:length(uniTempVec)){
  subTobeSearched <- substr(uniTempVec[i], 1,3)
  subTobeSearched <- paste0("^",subTobeSearched)
  tempVec <- unique(grep(subTobeSearched, uniTempVec, ignore.case = T, value = T))
  if(length(tempVec) > 1){
    tempListVec <- c(tempListVec, paste(" ", tempVec, collapse = " "))
  }
}

unitempListVec <- unique(tempListVec)
temp3Vec <- character()
for(i in 1:length(unitempListVec)){
  temp3Vec <- character()
  temp2Vec <- as.vector(unlist(strsplit(trimws(unitempListVec[i], "l"), "   ")))
  for (j in 1:length(temp2Vec)){
    temp3Vec <- c(temp3Vec,unique(as.character(BrDF[BrDF$City == temp2Vec[j], "State"])))
  }
  if(length(unique(temp3Vec)) != length(temp3Vec)){
    print(as.data.frame(sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))))
  }
}
```

```
##             sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Minneapolis                                                                  MN
## Minnetonka                                                                   MN
##                 sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## San Diego                                                                        CA
## San Francisco                                                                    CA
## San Antonio                                                                      TX
## Santa Cruz                                                                       CA
## Santee                                                                           CA
## San Luis Obispo                                                                  CA
## Santa Fe                                                                         NM
##   Marquette Martinsville Marietta Marlborough
## 1        MI           IN       GA          MA
## 2        MA           IN       GA          MA
##                 sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Brooklyn                                                                         NY
## Brooklyn Center                                                                  MN
## Broomfield                                                                       CO
## Bronx                                                                            NY
##              sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Westerly                                                                      RI
## Weston                                                                        MO
## West Chester                                                                  PA
## Westminster                                                                   MA
## Westfield                                                                     MA
##   Newport New.Orleans Newburgh New.York Newburyport
## 1      RI          LA       NY       NY          MA
## 2      OR          LA       NY       NY          MA
##           sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Lone Tree                                                                  CO
## Longmont                                                                   CO
##                 sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## North Woodstock                                                                  NH
## Norfolk                                                                          VA
## Northamtpon                                                                      MA
## North Conway                                                                     NH
##   Ashland Ashburn Asheville
## 1      VA      VA        NC
## 2      OR      VA        NC
##           sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Tampa                                                                      FL
## Tampa Bay                                                                  FL
##           sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Lansdale                                                                   PA
## Lancaster                                                                  PA
##   Jackson Jacksonville Jacksonville.Beach
## 1      WY           FL                 FL
## 2      MS           FL                 FL
##           sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Menominee                                                                  WI
## Menominie                                                                  WI
```


```r
## Do corrections to City Names or State Names
# COrrections 
BrDF[BrDF$City == "Menominee", 'City'] <- "Menomonie"
BrDF[BrDF$City == "Menominie", 'City'] <- "Menomonie"
# Remove leading white space from State col
BrDF$State <- trimws(BrDF$State, "l")
# City Marquette is in MI and not MA
BrDF[BrDF$City == "Marquette" & BrDF$State == "MA", 'State'] <- "MI"
```


```r
# Match Last 3 Chars
uniTempVec <- unique(BrDF$City)
tempListVec <- character()
for(i in 1:length(uniTempVec)){
  subTobeSearched <- substr(uniTempVec[i], nchar(uniTempVec[i]) - 3, nchar(uniTempVec[i]))
  subTobeSearched <- paste0(subTobeSearched,"$")
  tempVec <- unique(grep(subTobeSearched, uniTempVec,ignore.case = T, value = T))
  if(length(tempVec) > 1){
    tempListVec <- c(tempListVec, paste(" ", tempVec, collapse = " "))
  }
}

unitempListVec <- unique(tempListVec)
temp3Vec <- character()
for(i in 1:length(unitempListVec)){
  temp3Vec <- character()
  temp2Vec <- as.vector(unlist(strsplit(trimws(unitempListVec[i], "l"), "   ")))
  for (j in 1:length(temp2Vec)){
    temp3Vec <- c(temp3Vec,unique(as.character(BrDF[BrDF$City == temp2Vec[j], "State"])))
  }
  if(length(unique(temp3Vec)) != length(temp3Vec)){
    print(as.data.frame(sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))))
  }
}
```

```
##   Louisville Evansville Martinsville Bargersville Troutville Roseville
## 1         KY         IN           IN           IN         VA        MN
## 2         KY         IN           IN           IN         VA        MN
##   Warrenville Phoenixville Boonville Waynesville Charlottesville
## 1          IL           PA        CA          NC              VA
## 2          IL           PA        CA          NC              VA
##   Greenville Danville Woodinville Nashville Meadville Shelbyville
## 1         SC       PA          WA        TN        PA          IN
## 2         DE       PA          WA        TN        PA          IN
##   Jacksonville Hudsonville Huntsville Biglerville Asheville Gainesville
## 1           FL          MI         AL          PA        NC          FL
## 2           FL          MI         AL          PA        NC          FL
##   Knoxville Stevensville Mooresville Libertyville Garrattsville
## 1        IA           MT          NC           IL            NY
## 2        IA           MT          NC           IL            NY
##                     sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## San Francisco                                                                        CA
## South San Francisco                                                                  CA
##   Holland Portland Pineland Ashland Loveland Cleveland Lockland Cortland
## 1      MI       ME       ME      VA       CO        OH       OH       NE
## 2      MI       OR       ME      OR       CO        OH       OH       NE
##   Plainfield East.Fairfield Bloomfield Springfield Broomfield
## 1         IN             VT         CT          MO         CO
## 2         IN             VT         CT          OR         CO
##   South.Deerfield Westfield Sheffield
## 1              MA        MA        MA
## 2              MA        MA        MA
##                  sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Michigan City                                                                     IN
## Kansas City                                                                       MO
## Traverse City                                                                     MI
## Salt Lake City                                                                    UT
## Garden City                                                                       ID
## Oklahoma City                                                                     OK
## Lake Havasu City                                                                  AZ
## Plant City                                                                        FL
## Midwest City                                                                      OK
##                  sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Dripping Springs                                                                  TX
## Idaho Springs                                                                     CO
## Diamond Springs                                                                   CA
## Colorado Springs                                                                  CO
## Abita Springs                                                                     LA
##           sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Lemont                                                                     IL
## Longmont                                                                   CO
## Claremont                                                                  CA
## Belmont                                                                    CA
##   Bloomington Lexington Washington South.Burlington Wilmington Burlington
## 1          IL        KY         DC               VT         DE         VT
## 2          IN        VA         DC               VT         DE         VT
##   Lake.Barrington Arrington Covington
## 1              IL        VA        LA
## 2              IL        VA        LA
##              sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Austin                                                                        TX
## South Austin                                                                  TX
##   West.Chester Rochester Gloucester Westminster Worcester Leominster
## 1           PA        MI         MA          MA        MA         MA
## 2           PA        NY         MA          MA        MA         MA
##   Lancaster
## 1        PA
## 2        PA
##            sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Nellysford                                                                  VA
## Stamford                                                                    CT
## Stratford                                                                   CT
## Medford                                                                     OR
##            sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Waterbury                                                                   VT
## Middlebury                                                                  VT
##                    sapply(temp2Vec, function(x) unique(BrDF[BrDF$City == x, "State"]))
## Virginia Beach                                                                      VA
## Boynton Beach                                                                       FL
## Jacksonville Beach                                                                  FL
## Myrtle Beach                                                                        SC
```

```r
# No corrections after last 3 match
```


```r
as.data.frame(table(BrDF$Name))[as.data.frame(table(BrDF$Name))$Freq > 1, ]
```

```
##                        Var1 Freq
## 71       Blackrocks Brewery    2
## 75    Blue Mountain Brewery    2
## 298 Lucette Brewing Company    2
## 368     Oskar Blues Brewery    2
## 370     Otter Creek Brewing    2
## 443 Sly Fox Brewing Company    2
## 466  Summit Brewing Company    2
```

```r
# Remove Dups if needed
#BrDF <- BrDF[!duplicated(BrDF[,c("Name","City","State")]), ]
```


```r
## Look for duplicates in Brewry Name
## Match first 3
uniTempVec <- unique(BrDF$Name)
tempListVec <- character()
for(i in 1:length(uniTempVec)){
  subTobeSearched <- substr(uniTempVec[i], 1,3)
  subTobeSearched <- paste0("^",subTobeSearched)
  tempVec <- unique(grep(subTobeSearched, uniTempVec,ignore.case = T, value = T))
  if(length(tempVec) > 1){
    tempListVec <- c(tempListVec, paste(" ", tempVec, collapse = " "))
  }
}

unitempListVec <- unique(tempListVec)
temp3Vec <- character()
for(i in 1:length(unitempListVec)){
  temp3Vec <- character()
  temp2Vec <- trimws(as.vector(unlist(strsplit(trimws(unitempListVec[i], "l"), "   "))), "l")
  for (j in 1:length(temp2Vec)){
    temp3Vec <- c(temp3Vec,unique(paste(BrDF[BrDF$Name == temp2Vec[j], "City"], BrDF[BrDF$Name == temp2Vec[j], "State"], sep = "+")))
  }
  if(length(unique(temp3Vec)) != length(temp3Vec)){
    print(as.data.frame(sapply(temp2Vec, function(x) paste(BrDF[BrDF$Name == x, "City"], BrDF[BrDF$Name == x, "State"], sep = "+"))))
  }
}
```

```
##                           sapply(temp2Vec, function(x) paste(BrDF[BrDF$Name == x, "City"], BrDF[BrDF$Name == x, "State"], sep = "+"))
## Against the Grain Brewery                                                                                               Louisville+KY
## Against The Grain Brewery                                                                                               Louisville+KY
##                                   sapply(temp2Vec, function(x) paste(BrDF[BrDF$Name == x, "City"], BrDF[BrDF$Name == x, "State"], sep = "+"))
## Great Divide Brewing Company                                                                                                        Denver+CO
## Grey Sail Brewing Company                                                                                                         Westerly+RI
## Greenbrier Valley Brewing Company                                                                                                Lewisburg+WV
## Great Crescent Brewery                                                                                                              Aurora+IN
## Great Raft Brewing Company                                                                                                      Shreveport+LA
## Great River Brewery                                                                                                              Davenport+IA
## Grey Sail Brewing of Rhode Island                                                                                                 Westerly+RI
## Green Room Brewing                                                                                                            Jacksonville+FL
## Great Northern Brewing Company                                                                                                   Whitefish+MT
##                                 sapply(temp2Vec, function(x) paste(BrDF[BrDF$Name == x, "City"], BrDF[BrDF$Name == x, "State"], sep = "+"))
## The Mitten Brewing Company                                                                                                  Grand Rapids+MI
## The Dudes' Brewing Company                                                                                                      Torrance+CA
## The Lion Brewery                                                                                                            Wilkes-Barre+PA
## The Brewer's Art                                                                                                               Baltimore+MD
## The Alchemist                                                                                                                  Waterbury+VT
## The Just Beer Project                                                                                                         Burlington+VT
## The Bronx Brewery                                                                                                                  Bronx+NY
## The Traveler Beer Company                                                                                                     Burlington+VT
## The Right Brain Brewery                                                                                                    Traverse City+MI
## The Black Tooth Brewing Company                                                                                                 Sheridan+WY
## The Manhattan Brewing Company                                                                                                   New York+NY
##                               sapply(temp2Vec, function(x) paste(BrDF[BrDF$Name == x, "City"], BrDF[BrDF$Name == x, "State"], sep = "+"))
## Hopworks Urban Brewery                                                                                                        Portland+OR
## Hops & Grains Brewing Company                                                                                                   Austin+TX
## Hop Valley Brewing Company                                                                                                 Springfield+OR
## Hop Farm Brewing Company                                                                                                    Pittsburgh+PA
## Hops & Grain Brewery                                                                                                            Austin+TX
##                              sapply(temp2Vec, function(x) paste(BrDF[BrDF$Name == x, "City"], BrDF[BrDF$Name == x, "State"], sep = "+"))
## Goose Island Brewing Company                                                                                                  Chicago+IL
## Goodlife Brewing Co.                                                                                                             Bend+OR
## Goose Island Brewery Company                                                                                                  Chicago+IL
## Good Life Brewing Company                                                                                                        Bend+OR
## Good People Brewing Company                                                                                                Birmingham+AL
##                                sapply(temp2Vec, function(x) paste(BrDF[BrDF$Name == x, "City"], BrDF[BrDF$Name == x, "State"], sep = "+"))
## Catawba Valley Brewing Company                                                                                                Morganton+NC
## Catawba Island Brewing                                                                                                     Port Clinton+OH
## Catawba Brewing Company                                                                                                       Morganton+NC
##                              sapply(temp2Vec, function(x) paste(BrDF[BrDF$Name == x, "City"], BrDF[BrDF$Name == x, "State"], sep = "+"))
## Angry Minnow                                                                                                                  Hayward+WI
## Angry Orchard Cider Company                                                                                                Cincinnati+OH
## Angry Minnow Brewing Company                                                                                                  Hayward+WI
```


```r
# Do corrections here
BrDF[BrDF$Name == "Against the Grain Brewery", "Name"] <- "Against The Grain Brewery"
BrDF[BrDF$Name == "Grey Sail Brewing of Rhode Island", "Name"] <- "Grey Sail Brewing Company"
BrDF[BrDF$Name == "Hops & Grains Brewing Company", "Name"] <- "Hops & Grain Brewery"
BrDF[BrDF$Name == "Goose Island Brewery Company", "Name"] <- "Goose Island Brewing Company"
BrDF[BrDF$Name == "Goodlife Brewing Co.", "Name"] <- "Good Life Brewing Company"
BrDF[BrDF$Name == "Catawba Brewing Company", "Name"] <- "Catawba Valley Brewing Company"
BrDF[BrDF$Name == "Angry Minnow", "Name"] <- "Angry Minnow Brewing Company"

#Write Clean File
write.csv(BrDF, file = "CleanedBreweryData.csv", row.names = F)
```

#Guiding Questions

###First 6, Last 6
The files were merged and some of the observations are seen below.  The data gathered includes... 


```r
#Q2 - Merge Beer and Breweries files by Brew_ID (Assumes Beers.csv file has renamed Brewery_ID to Brew_ID) and prints the first/last 6 observations.
beers <- read.csv("CleanedBeerData.csv", header = TRUE)
breweries <- read.csv("CleanedBreweryData.csv", header = TRUE)
master.file <- Reduce(function(beers, breweries) merge(beers, breweries, by="Brew_ID"), list(beers, breweries))
names(master.file) <- c("Brewery_id", "Beer_name", "Beer_id", "ABV", "IBU", "Style", "Ounces", "Brewery_name", "City", "State")
write.csv(master.file, file = "masterfile.csv", row.names = FALSE)
head(master.file,6)
```

```
##   Brewery_id     Beer_name Beer_id   ABV IBU
## 1          1  Get Together    2692 0.045  50
## 2          1 Maggie's Leap    2691 0.049  26
## 3          1    Wall's End    2690 0.048  19
## 4          1       Pumpion    2689 0.060  38
## 5          1    Stronghold    2688 0.060  25
## 6          1   Parapet ESB    2687 0.056  47
##                                 Style Ounces       Brewery_name
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
##      Brewery_id                 Beer_name Beer_id   ABV IBU
## 2361        556             Pilsner Ukiah      98 0.055  NA
## 2362        557  Heinnieweisse Weissebier      52 0.049  NA
## 2363        557           Snapperhead IPA      51 0.068  NA
## 2364        557         Moo Thunder Stout      50 0.049  NA
## 2365        557         Porkslap Pale Ale      49 0.043  NA
## 2366        558 Urban Wilderness Pale Ale      30 0.049  NA
##                        Style Ounces                  Brewery_name
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
Once the data was cleaned and combined, there were still data that showed incomplete.  These include 997 IBU ratings and 62 ABV marks, as seen below.


```r
#Q3 - Summing the NA's for each column
colSums(is.na(master.file))
```

```
##   Brewery_id    Beer_name      Beer_id          ABV          IBU 
##            0            0            0           62          997 
##        Style       Ounces Brewery_name         City        State 
##            0            0            0            0            0
```

###Breweries per state
The 558 breweries are spread among all 50 states and includes District of Columbia for a total of 51.  The most breweries are located in in Colorado (259).  Other states with at least 100 breweries include: Pennsylvania (100), Oregon (114), Texas (130), Indiana (138), Michigan (162), and California (181).  

```r
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


###Median Alcohol Content & International Bitterness Units (IBU) for each state
The median amount of alcohol content and IBUs in each state is listed below. "Add Info about data"

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


