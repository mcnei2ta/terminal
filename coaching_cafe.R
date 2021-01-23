### Unused, useful functions
# distinct, pattern, rbind, %in%, mutate

# Clear environment
rm(list = ls())
# Clear console
cat("\014")

library(tidyr) ## spread
library(dplyr) ## mutate, filter, select, distinct, group_by, summarize
library(data.table) ## fread, pattern, merge, rbind, setorder
library(ggplot2) ## for plots
library(cowplot) ## print plots side by side
library(openxlsx) ## creating excel workbooks

# Note: there is a package to hit databases directly like SQL, but we don't use it at STU. 
# I use R for standard data sets that have already been pulled and to merge those with external data sets (from CBP, USCIS, EOIR, etc)

setwd("/Users/thomasmcneill/Documents/data/coaching_cafe")

# Read in our data sets: 
#   1. Vehicle listings within 50 miles of my zip code 
#   2. Ratings for different models

# simple method
vehicle_listings <- fread("vehicle_listings.csv", check.names = T)
vehicle_ratings <- fread("vehicle_ratings.csv", check.names = T)

rm(list = ls(pattern = "vehicle*"))

# efficient method
# create a list of the names of all files in the working directory, call the list "files"
files <- list.files(pattern = "*csv")

# read in all the files in the working directory, name them according to their filenames
for(i in 1:length(files)){
  assign(files[i],fread(files[i], check.names = TRUE))
}

# list all columns in our data sets
glimpse(vehicle_listings.csv)
glimpse(vehicle_ratings.csv)

###### combining data sets (a SQL left join)

# the pattern of the "car" column from ratings can be used as an identifier for each model/year
# create the same column for the listings df so we can join the ratings to the listing df

#### create new combined column for brand and model in same format as appears in kbb urls (to join)
vehicle_listings.csv$car <- paste0(
  tolower(gsub(" ","-", vehicle_listings.csv$brand)),
  "/",
  tolower(gsub(" ","-", vehicle_listings.csv$model)),
  "/",
  tolower(gsub(" ","-", vehicle_listings.csv$productionYear)),
  "/")

# Now we can join add ratings to the listings df
#### merge dataframes (same as a left join from listings to kbb)
listings <- merge(vehicle_listings.csv, vehicle_ratings.csv, by = "car", all.x = TRUE)

# Clean up environment
rm(list=ls(pattern = '*csv'))

### see every column in the data frame
glimpse(listings)
# We can also view the df in a new tab 
View(listings)
# also do this by clicking df in environment

# Odometer was read in as a character; change it to numeric (remove commas first)
listings$Odometer <- as.numeric(gsub(",","", listings$Odometer))

# select columns to keep
listings <- listings %>%
  select(manufacturer, model, productionYear, bodyType, price, Odometer, color, expert_rating, 
         consumer_rating, itemCondition, driveWheelConfiguration, vehicleInteriorColor, 
         fuelType, fuelEfficiency, vehicleEngine, vehicleTransmission) %>%
  setorder(-expert_rating, price)

###### EDA

# Now that we have our final dataset, we can perform some EDA

# First, let's create pivot tables

car_year <- listings %>%
  group_by(productionYear) %>%        # rows (break out by production year)
  summarise(num_listings=n()) %>%     # count (total number or listings)
  setorder(-productionYear)           # order (production year descending)

# We can also add column breakouts for pivot tables
car_year_type <- listings %>%
  group_by(productionYear, bodyType) %>%        # select fields we want to use for rows/columns
  summarise(num_listings=n()) %>%               # count (total number or listings)
  spread(bodyType, num_listings, fill=0) %>%    # specify column and count, remaining field from "group_by" is row
  setorder(-productionYear)                     # order (production year descending)

# Create a few more pivot tables to use for plotting
# filter to cars produced 2010 or later for clearer plots

# total number of listings by manufacturer
car_count <- listings %>%
  filter(productionYear>2009) %>%
  group_by(manufacturer) %>%
  summarise(num_listings=n()) %>%
  filter(num_listings>2) %>%
  setorder(-num_listings)

# average price of cars by manufacturer
car_price <- listings %>%
  filter(price>0) %>%
  filter(productionYear>2009) %>%
  group_by(manufacturer) %>%
  summarise(avg_price=mean(price)) %>%    # We can also use average as the pivot count instead of total listings
  setorder(-avg_price)

# average expert rating by manufacturer
car_exp_rating <- listings %>%
  filter(productionYear>2009) %>%
  filter(!is.na(expert_rating)) %>%
  group_by(manufacturer) %>%
  summarise(avg_rating=mean(expert_rating)) %>%
  setorder(-avg_rating)

# average consumer rating by manufacturer
car_cons_rating <- listings %>%
  filter(productionYear>2009) %>%
  filter(!is.na(consumer_rating)) %>%
  group_by(manufacturer) %>%
  summarise(avg_rating=mean(consumer_rating)) %>%
  setorder(-avg_rating)

# total number of listing by body type
car_type <- listings %>%
  filter(!is.na(bodyType)) %>%
  filter(bodyType != "Unavailable ") %>%
  group_by(bodyType) %>%
  summarise(num_listings=n()) %>%
  setorder(-num_listings)

View(car_price)

##### Graphs in ggplot2

##### Distribution of prices (excluding prices of 0)
# we can get a better view of the dist if we exclude the outliers to the right
listings %>%
  filter(price>0) %>%
  filter(price<100000) %>%
  ggplot(aes(price/1000)) +
  geom_histogram(bins=100)
# The distribution skews right

##### counts and prices by manufacturer 
### manufacturer by number of listings
plot_count <- car_count %>%
  ggplot(aes(x=reorder(manufacturer, num_listings))) +
  geom_col(aes(y=num_listings)) +
  coord_flip() + 
  ggtitle("Number of Listings") +
  xlab("Manufacturer") + ylab("Number of Listings")

### manufacturer by average price
plot_price <- car_price %>%
  ggplot(aes(x=reorder(manufacturer, avg_price))) +
  geom_col(aes(y=avg_price)) + 
  coord_flip() + 
  ggtitle("Average Price") +
  xlab("Manufacturer") + ylab("Average Price")

plot_grid(plot_count, plot_price)      ## this function shows multiple graphs side by side

####### ratings by manufacturer
### manufacturer by average expert rating
plot_exp_rating <- car_exp_rating %>%
  ggplot(aes(x=reorder(manufacturer, avg_rating))) +
  geom_col(aes(y=avg_rating)) + 
  coord_flip() + 
  ggtitle("Expert Rating") +
  xlab("Manufacturer") + ylab("Avg Expert Rating")

plot_cons_rating <- car_cons_rating %>%
  ggplot(aes(x=reorder(manufacturer, avg_rating))) +
  geom_col(aes(y=avg_rating)) + 
  coord_flip() + 
  ggtitle("Consumer Rating") +
  xlab("Manufacturer") + ylab("Avg Consumer Rating")

plot_grid(plot_exp_rating, plot_cons_rating)

####### year and type by manufacturer 
### listings by body type
plot_type <- car_type %>%
  ggplot(aes(x=reorder(bodyType, num_listings))) +
  geom_col(aes(y=num_listings)) + 
  coord_flip() + 
  ggtitle("Body Type") +
  xlab("Body Type") + ylab("Number of Listings")

### listings by production year
plot_year <- car_year %>%
  filter(productionYear>1999) %>%
  ggplot(aes(x=productionYear)) +
  geom_col(aes(y=num_listings)) + 
  coord_flip() + 
  ggtitle("Production Year") +
  xlab("Production Year") + ylab("Number of Listings")

plot_grid(plot_type, plot_year)

# I'm not well versed in ggplot2 - these can be customized to look a lot better

brand_listings_price_ratings <- listings %>%
  filter(price>0) %>%
  filter(!is.na(expert_rating)) %>%
  filter(!is.na(consumer_rating)) %>%
  group_by(Manufacturer=manufacturer) %>%
  summarise(`Number of Listings`=n(), `Average Price`=mean(price), `Avg Expert Rating`=mean(expert_rating))

#################  Create excel report ################

######## EXCEL #########

## Create styles

Title <- createStyle(halign = "left", valign = "center", textDecoration = "bold")

Headers <- createStyle(fontColour = "#FFFFFF", border = c("top", "bottom", "left", "right"), 
                       fgFill = "#6495ED", halign = "center", valign = "center",
                       textDecoration = "bold")

Currency <- createStyle(numFmt = '_($* #,##0_);_($* (#,##0);_($* "-"??_);_(@_)', border = c("top", "bottom", "left", "right"),
                       halign = "right", valign = "center")

Counts <- createStyle(numFmt = 'COMMA', border = c("top", "bottom", "left", "right"),
                        halign = "right", valign = "center")

Ratings <- createStyle(numFmt = '0.0', border = c("top", "bottom", "left", "right"),
                       halign = "right", valign = "center")

Total <- createStyle(border = c("top", "bottom", "left", "right"), 
                     fgFill = "#C0C0C0", halign = "center", valign = "center")

Double_Border <- createStyle(border = "bottom", borderStyle = "double")

## Create a new workbook

wb <- createWorkbook(creator = "Thomas"
                     , title = "Vehicles for Sale"
                     , subject = "Within 50 miles of Richmond, VA")

addWorksheet(wb, "Summary")

# FOOTNOTES
long_date <- format(Sys.Date(),'%A, %B %d, %Y')

footnote <- paste0("Vehicles for sale in my area as of ", Sys.Date())

writeData(wb,"Summary", footnote, startCol = 1, startRow = 1)

### Summary Tab

# Table: Vehicle Listings by Manufacturer
# Title
writeData(wb,"Summary", "VEHICLE LISTINGS, PRICES AND RATINGS BY MANUFACTURER", startCol = 1, startRow = 3)
addStyle(wb, "Summary", Title, rows=3, cols=1, gridExpand = FALSE, stack = FALSE)

# Data
writeData(wb,"Summary", brand_listings_price_ratings, startCol = 1, startRow = 4, borders = "all")

# formatting
addStyle(wb, "Summary", Headers, cols=1:4, rows=4, gridExpand = FALSE, stack = FALSE)
addStyle(wb, "Summary", Counts, rows=5:(4+nrow(brand_listings_price_ratings)), cols=2, gridExpand = TRUE, stack = FALSE)
addStyle(wb, "Summary", Currency, rows=5:(4+nrow(brand_listings_price_ratings)), cols=3, gridExpand = TRUE, stack = FALSE)
addStyle(wb, "Summary", Ratings, rows=5:(4+nrow(brand_listings_price_ratings)), cols=4, gridExpand = TRUE, stack = FALSE)

# Color scales
# conditionalFormatting(wb, "Summary", cols=2, rows=4:(4+nrow(brand_listings_price_ratings)), style = c("#F8696B", "#FFEB84", "#63BE7B"), type = "colourScale")
conditionalFormatting(wb, "Summary", cols=3, rows=4:(4+nrow(brand_listings_price_ratings)), style = c("#63BE7B", "#FFEB84", "#F8696B"), type = "colourScale")
conditionalFormatting(wb, "Summary", cols=4, rows=4:(4+nrow(brand_listings_price_ratings)), style = c("#F8696B", "#FFEB84", "#63BE7B"), type = "colourScale")

# Gridlines
showGridLines(wb, "Summary", showGridLines = FALSE)

## Tab column widths
setColWidths(wb, "Summary", cols=1:4, widths = 16)

### Save the workbook
saveWorkbook(wb, paste0("Vehicle_Listings",Sys.Date(), ".xlsx"), overwrite = TRUE)
