# Clear environment
rm(list = ls())
# Clear console
cat("\014")

library(rvest)
library(tidyr)
library(plyr)
library(dplyr)
library(data.table)

setwd("/Users/thomasmcneill/Documents/data")

####### Read in listings from first page of search: richmond cars on autotrade

# load in HTML from URL
url <- "https://www.zillow.com/homes/Richmond,-VA_rb/"
page <- read_html(url)

# get the desired text from the html
address <- page %>%
  html_nodes("address") %>%
  html_text()
address <- as.data.frame(address)

price <- page %>%
  html_nodes("div") %>%
  html_text()
price <- as.data.frame(price) %>%
  filter(grepl('\\$', price)) %>%
  filter(!grepl('sqft', price)) %>%
  filter(!grepl('Price', price)) %>%
  filter(!grepl('Home', price)) %>%
  filter(!grepl('acres', price))%>%
  filter(!grepl('sale', price))

# URL Pattern
beg_url <- "https://www.autotrader.com/cars-for-sale/all-cars/"
# [Price Range]
# /
# [Production Year]
end_url <- "/richmond-va-23220?year=&channel=ATC&dma=&searchRadius=50&isNewSearch=false&marketExtension=include&showAccelerateBanner=false&sortBy=relevance&numRecords=100"
