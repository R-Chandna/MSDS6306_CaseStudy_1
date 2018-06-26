# MSDS6306_CaseStudy_1

## Data sets
* Beers.csv 2410 craft beers from US breweries
* Breweries.csv 558 US breweries

## Authors
* Rajat Chandna
  rchandna@mail.smu.edu
* Andy Ho
  atho@mail.smu.edu
* An Nguyen
  anguyen2@mail.smu.edu
* Jodi Pafford
  jpafford@mail.smu.edu
* Tori Wheelis
  twheelis@mail.smu.edu

## Member Assignments
* For the preparation:
	* Data Tidyng: with Rajat's assistance
		* Beers: An, Jodi, some help from Tori
		* Breweries: Rajat and Andy
	* R-code for questions:
		* 1-3: Andy
		* 4-7: Tori
	* Presentation:
		* Created by Jodi with help from Tori
* For the presentation:
	* Intro: Tori
	* Tidying data process:
		* Beers: An and Jodi
		* Breweries: Rajat and Andy
	* Answers to Questions:
		* 1-3: Andy
		* 4-7: Tori
	* Closing and Q&A: Jodi

## Project Details
* The "story" created to go with this project was that TAJAR, Inc., our imaginary data company, was tasked with helping Brewing Bros, an international brewing company looking to expand to the US, to learn more about the US beer brewing market, about the beers that are popular, and get suggestions on where to set up their next brewery and suggestions on beers to feature in that brewery. 
* We pretended that the (US) National Beer and Wine Association works closely with the largest beer distributors and had gathered data on their most popular beers (and the breweries they came from) and breweries, which they then let us use.
* After the data cleaning process, detailed in the codebook (and also illustrated in the echoed code chunks in the deliverable), we ran a few analyses to help us understand some basic statistics about the US beer market.
* Our "report" for Brewing Bros details what we learned, and our accompanying powerpoint corresponds to our report.

## Data Tidying
* Data set of 2410 craft beers from US breweries and 558 US breweries obtained from Dr. Jacquelyn Cheun from the Masters of Science in Data Science of the Southern Methodist University.

### INFORMATION ON CRAFT BEERS DATASET (Beers.csv)
* Beers.csv is UTF-8 encoded
	* Some beer names included year of production, these beers are assumed to be different from beers with the same name but no production year
	* Duplicates removed, the criteria for removal is that all variables except Beer_ID matched.
		* Additionally Ranger IPA (Current)
		* Shift (1)
		* Point Special (Current)
		* Cherry Ale (1)
		* Pleasure Town
		* Farmer Ted's Cream Ale
		* Dry Cider, Abbey's Single (2015- )
		* Triomphe, Bender
		* Hipster Ale (Westbrook Brewing) <br /> 
		were found to be duplicates of other beers but with Name field slightly different, these beers were also removed.
		* Hipster Ale (Two Roads Brewing) was renamed to Hipster Ale because Two Roads Brewing does not brew this beer
	* Cleaned file is output to "CleanedBeerData.csv"

#### CODEBOOK FOR CRAFT BEERS DATASET

|VARIABLE NAME|POSITION|VARIABLE|
|:-|:-:|-:|
|Name|1|Name of beer|
|Beer_ID|2|Beer's ID number|	
|ABV|3|Alcohol by volume|	
|IBU|4|International bitterness units|	
|Breweriy_id|5|Beer's brewery's ID|	
|Style|6|Style of beer|
|Ounces|7|Volume of beer per bottle in ounces|	

### INFORMATION ON US BREWERIES DATASET (Breweries.csv)
* 51 states:  50 US states plus Washington DC
* Trimmed whitespaces infront of all State initials
* Abbreviation of city names were expanded
* Mispellings were corrected
* Marquette was listed in MA, corrected to MI
	* "Against the Grain Brewery" changed to "Against The Grain Brewery"
	* "Grey Sail Brewing of Rhode Island" changed to "Grey Sail Brewing Company"
	* "Hops & Grains Brewing Company", "Name"] changed to "Hops & Grain Brewery"
	* "Goose Island Brewery Company", "Name"] changed to "Goose Island Brewing Company"
	* "Goodlife Brewing Co.", "Name"] changed "Good Life Brewing Company"
	* "Catawba Brewing Company", "Name"] changed to "Catawba Valley Brewing Company"
	* Angry Minnow", "Name"] <- "Angry Minnow Brewing Company"
* Cleaned file is ouput to "CleanedBreweryData.csv"

#### CODEBOOK FOR BREWERIES DATASET:

|VARIABLE NAME|POSITION|VARIABLE|
|:-|:-:|-:|
|Brew_ID|1|Brewery's ID|
|Name|2|Name of brewery|
|City|3|City of brewery's location|
|State|4|State of brewery's locaion (in standard post office state abbreviations)|

### CODEBOOK FOR MERGED BEERS and BREWERY DATA (masterfile.csv)

|VARIABLE NAME|POSITION|VARIABLE|
|:-|:-:|-:|
|Brewery_id|1|Brewery's ID|
|Beer_name|2|Name of beer|
|Beer_id|3|Beer's ID number|
|ABV|4|Alcohol by volume|
|IBU|5|International bitterness units|
|Style|6|Style of beer|
|Ounces|7|Volume of beer per bottle in ounces|
|Brewery_name|8|Name of brewery|
|City|9|City of brewery's location|
|State|10|State of brewery's locaion (in standard post office state abbreviations)|
