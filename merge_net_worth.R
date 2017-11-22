library(tidyverse)
library(readr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)


setwd("/Users/jasmindial/Desktop/D3-Inequality-/")

wealth <- read_csv("wealth-tables-2013.csv", skip = 5)[1:7,1:2] 
colnames(wealth)[1:2] <- c("Race_Ethnicity", "2013")
#maybe just give column a list
wealth$Race_Ethnicity[2] <- "white, not hispanic"
wealth$Race_Ethnicity[6] <- "hispanic origin"
wealth <- mutate(wealth, Race_Ethnicity = tolower(Race_Ethnicity))

#wealth2 <- read_csv("Wealth_Tables_2002.csv", skip = 3)[1:6,1:2] 


for (csv in c("Wealth_Tables_2011.csv", "Wealth_Tables_2010.csv", "wealth-tables-2009.csv",
              "Wealth_Tables_2005.csv", "Wealth_Tables_2004.csv", "Wealth_Tables_2002.csv")) {
  year <- substr(csv, 15, 18)
  
  if (year == "2004") {
    wealth2 <- read_csv(csv, skip = 3)[1:7,1:2]
  } else if (year == "2002") {
     wealth2 <- read_csv(csv, skip = 3)[1:6,1:2] 
  } else {
  wealth2 <- read_csv(csv, skip = 5)[1:7,1:2]
  }
  
  colnames(wealth2)[1:2] <- c("Race_Ethnicity", year)
  wealth2$Race_Ethnicity[2] <- "white, not hispanic"
  
  if (year == "2002") {
    wealth2$Race_Ethnicity[1] <- "white alone"
    wealth2$Race_Ethnicity[3] <- "black alone"
    wealth2$Race_Ethnicity[6] <- "asian alone"
  } else {
  wealth2$Race_Ethnicity[6] <- "hispanic origin" 
  }
  
  wealth2 <- mutate(wealth2, Race_Ethnicity = tolower(Race_Ethnicity))

  wealth <- full_join(wealth, wealth2)
}
  