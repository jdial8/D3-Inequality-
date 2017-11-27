##### Data Source: https://www.census.gov/topics/income-poverty/wealth/data/tables.All.html #####

library(tidyverse)
library(readr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)
library(ggplot2)



setwd("/Users/jasmindial/Desktop/D3-Inequality-/")

wealth <- read_csv("wealth-tables-2013.csv", skip = 5)[1:7,1:2] 
colnames(wealth)[1:2] <- c("Race_Ethnicity", "2013")

#maybe just give column a list - always in same order? 
wealth$Race_Ethnicity[2] <- "white, not hispanic"
wealth$Race_Ethnicity[6] <- "hispanic origin"
wealth <- mutate(wealth, Race_Ethnicity = tolower(Race_Ethnicity))

#test <- read_csv("Wealth_Tables_2002.csv", skip = 3)[1:6,1:2] 

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
  

#convert wide to long w/ year first 
wealth$Race_Ethnicity <- c("white", "white_not_h", "black", "asian", "other", "hispanic", "not_h")
long_wealth <- gather(wealth, year, amount, `2013`:`2002`, factor_key=TRUE) %>% 
                select(year, Race_Ethnicity, amount) %>%
                filter(Race_Ethnicity %in% c("white_not_h", "black", "asian", "hispanic"))

json <- toJSON(long_wealth, pretty=TRUE)
write(json, file="networth2.json")

#transpose data to prep for line graph (instead of long version)
#cols <- wealth$Race_Ethnicity
#keys <- colnames(wealth)[2:8]
#wealth <- as.tibble(t(wealth[,-1]))
#wealth <- mutate(wealth, year = keys)
#colnames(wealth) <- c("white", "white_not_h", "black", "asian", "other", "hispanic", "not_h", "year")
#wealth <- subset(wealth, select = -c(white, other, not_h))


#write to json
#json <- toJSON(wealth, pretty=TRUE)
#write(json, file="Networth.json")


#ggplot(data=long_wealth, aes(x = year, y = amount, color = Race_Ethnicity, group = Race_Ethnicity)) +
  #geom_point(size=0.5) +
  #geom_line()




### Distribution Data ### -------------------------------------------------------------------------------


wealth_dist <- NULL

for (csv in c("wealth-dist-2013.csv", "Wealth_dist_2011.csv", "Wealth_dist_2010.csv", 
              "wealth_dist_2005.csv", "Wealth_dist_2004.csv", "wealth_dist_2002.csv")) {
  year <- substr(csv, 13, 16)
  if (year == "2011") {
    dist <- read_csv(csv, skip = 4)[3:9, 1:11]
  } else if (year %in% c("2004", "2002")) {
    dist <- read_csv(csv, skip = 1)[3:9, 1:11] 
  } else {
    dist <- read_csv(csv, skip = 3)[3:9, 1:11]
  }
  
  colnames(dist)[1:2] <- c("Race_Ethnicity", "total")
  dist <- subset(dist, select = -c(total))
  dist$Race_Ethnicity <- c("white", "white_not_h", "black", "asian", "other", "hispanic", "not_h")
  dist2 <- gather(dist, bucket, percent, `Zero or Negative`:`$500,000 or over`) %>%
    mutate(year = year) %>%
    filter(Race_Ethnicity %in% c("white_not_h", "black", "asian", "hispanic"))
  
  wealth_dist <- rbind(wealth_dist, dist2)
}


dist <- read_csv("wealth_dist_2002.csv", skip = 1)[3:9, 1:11]
colnames(dist)[1:2] <- c("Race_Ethnicity", "total")
dist <- subset(dist, select = -c(total))
year <- substr("Wealth_dist_2010.csv", 13, 16)
dist$Race_Ethnicity <- c("white", "white_not_h", "black", "asian", "other", "hispanic", "not_h")
hmm <- gather(dist, bucket, percent, `Zero or Negative`:`$500,000 or over`) %>%
        mutate(year = year) 
                                #%>%
       # rename("Race_Ethnicity" = "X1")


json2 <- toJSON(wealth_dist, pretty=TRUE)




#later: include the totals?


