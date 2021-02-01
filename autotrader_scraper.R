# Clear environment
rm(list = ls())
# Clear console
cat("\014")

# TO UPDATE THIS SCRIPT FOR A DIFFERENT ZIP CODE:
# 1. The only lines that should need to be updated are 46, 63, and 235 (these are URLs - whole URLs or sections)
# 2. On the Autotrader homepage, search your zip code (any make/model), and copy the URL. Replace the lines above with it, 
# making sure your including the right sections (or the whole URL if applicable)
# 3. Search and replace "Richmond" with your area
# 4. If you live in a populated area with a lot of cars for sale, I would change the search radius to 10 miles instead of 50
# as you'll see below, each search we loop through has to have <1000 results in order to capture all vehicles for sale, and 
# a lower radius makes this more likely. If any searches result in >1000 results, the script should still work, it just won't
# pull back all listings 

### autotrader only lets you see the first 1000 results of any search
# Therefore each search needs to be broken into sections resulting in <1000 results. 
# To achieve this for the Richmond area, searches are broken down in the following manner:
# 1. Production Years: This Year - Five Years Ago by Price Range (Ten Price Ranges)
# 2. Production Years: Six - Thirteen Years Ago by Year
# 3. Production Years: 14 - 40 Years Ago

# fewer results could also be achieved by adjusting the searchRadius in the URL (currently 50 mile radius)

### TROUBLESHOOTING
# If you get this error in for loop: Error in if (results == "0 Results") { : argument is of length zero
# To fix: Update the selector path (may have been changed by Autotrader developers)
#     Go to URL > Developer > Show Page Source
#     Search the page source for the text showing number of results
#     Right click this line > Copy > Selector Path
#     Replace the selector path in: results -> page %>% html_nodes("")

library(rvest)
library(tidyr)
library(plyr)
library(dplyr)
library(data.table)

setwd("/Users/thomasmcneill/Documents/data")

####### Read in listings from first page of search: richmond cars on autotrader
## (we need to make a non-empty data frame that we can add more listings to in the following for loops; so we start by creating 
## a dataframe of the results from the first page of results.)

# load in HTML from URL
url <- "https://www.autotrader.com/cars-for-sale/all-cars/richmond-va-23220?dma=&searchRadius=50&marketExtension=off&isNewSearch=false&showAccelerateBanner=false&sortBy=relevance&numRecords=100"
page <- read_html(url)

# get the desired text from the html
text <- page %>%
  html_nodes("script") %>%
  html_text()

# write as dataframe
df <- as.data.frame(text) %>%
  filter(grepl('type":"Car', text))

# URL Pattern
beg_url <- "https://www.autotrader.com/cars-for-sale/all-cars/"
# [Price Range]
# /
# [Production Year]
end_url <- "/richmond-va-23220?year=&channel=ATC&dma=&searchRadius=50&isNewSearch=false&marketExtension=include&showAccelerateBanner=false&sortBy=relevance&numRecords=100"

################## 1. Production Years: This Year - Five Years Ago by Price Range (Ten Price Ranges)

# latest brands on market are current year +1
# create year range for this section
this_year <- year(Sys.Date())
five_years_back <- year(Sys.Date())-5

# create list of price ranges
price_ranges <- c("cars-under-18000",
                  "cars-between-18000-and-21000",
                  "cars-between-21000-and-24000",
                  "cars-between-24000-and-27000",
                  "cars-between-27000-and-30000",
                  "cars-between-30000-and-35000",
                  "cars-between-35000-and-41000",
                  "cars-between-41000-and-47000",
                  "cars-between-47000-and-55000",
                  "cars-over-55000")

# loop through combinations of production years and price ranges to search
for (year in this_year:five_years_back){
  for(pr in 1:length(price_ranges)){
    
    # create URL for this year and price range
    url <- paste0(beg_url, price_ranges[pr], "/", year, end_url)
    
    # read URL html
    page <- read_html(url)
    
    # get number of listings on this page
    results <- page %>%
      html_nodes("#mountNode > div:nth-child(1) > div.colored-background.inset.bg-gray-lightest.padding-top-4 > div > div.row.margin-horizontal-0.padding-horizontal-0 > div.row > div > div.display-flex.justify-content-between.align-items-center.justify-content-end.margin-bottom-2 > div.results-text-container.text-size-200") %>%
      html_text()
    
    # if there are no results, end loop
    if (results=="0 Results") {
      print(paste(results, " for", year, price_ranges[pr]))
    
    # if non-zero number of listings, add those on this page to the data frame
    } else {
      print(paste(results, " for", year, price_ranges[pr]))
      
      # get number of results as a numeric value
      results <- strsplit(results, split = "of")[[1]][2]
      results <- strsplit(results, split = "Results")
      results <- as.numeric(gsub(" ","", results[[1]][1]))
      
      # if the number of results is 100 or less, read results and move to next search
      if (results<101) {
        text <- page %>%
          html_nodes("script") %>%
          html_text()
        
        temp <- as.data.frame(text) %>%
          filter(grepl('type":"Car', text))
        df <- rbind.fill(df,temp)
        
        # if results > 100, read this page, then move to next page until you've read in the last page of listings for this search
      } else {
        
        # read in the first page of this search
        text <- page %>%
          html_nodes("script") %>%
          html_text()
        
        temp <- as.data.frame(text) %>%
          filter(grepl('type":"Car', text))
        df <- rbind.fill(df,temp)
        
        # determine the remaining number of pages for this search
        pages_left <- ceiling(results/100)-1
        
        # read in the remaining pages for this search
        for (page in 1:pages_left) {
          next_100 <- paste0("&firstRecord=",page*100)
          next_url <- paste0(url, next_100)
          
          page <- read_html(next_url)
          
          text <- page %>%
            html_nodes("script") %>%
            html_text()
          
          temp <- as.data.frame(text) %>%
            filter(grepl('type":"Car', text))
          df <- rbind.fill(df,temp)
          
        }
      }
    }
  }
}
################## 2. Production Years: Six - Thirteen Years Ago by Year
six_years_back <- year(Sys.Date())-6
thirteen_years_back <- year(Sys.Date())-13

for (year in six_years_back:thirteen_years_back){
  # create URL for this year and price range
  url <- paste0(beg_url, year, end_url)
  
  # read URL html
  page <- read_html(url)
  
  # get number of listings on this page
  results <- page %>%
    html_nodes("#mountNode > div:nth-child(1) > div.colored-background.inset.bg-gray-lightest.padding-top-4 > div > div.row.margin-horizontal-0.padding-horizontal-0 > div.row > div > div.display-flex.justify-content-between.align-items-center.justify-content-end.margin-bottom-2 > div.results-text-container.text-size-200") %>%
    html_text()
  
  # if there are no results, end loop
  if (results=="0 Results") {
    print(paste(results, " for", year))
    
    # if non-zero number of listings, add those on this page to the data frame
  } else {
    print(paste(results, " for", year))
    
    # get number of results as a numeric value
    results <- strsplit(results, split = "of")[[1]][2]
    results <- strsplit(results, split = "Results")
    results <- as.numeric(gsub(" ","", results[[1]][1]))
    
    # if the number of results is 100 or less, read results and move to next search
    if (results<101) {
      text <- page %>%
        html_nodes("script") %>%
        html_text()
      
      temp <- as.data.frame(text) %>%
        filter(grepl('type":"Car', text))
      df <- rbind.fill(df,temp)
      
      # if results > 100, read this page, then move to next page until you've read in the last page of listings for this search
    } else {
      
      # read in the first page of this search
      text <- page %>%
        html_nodes("script") %>%
        html_text()
      
      temp <- as.data.frame(text) %>%
        filter(grepl('type":"Car', text))
      df <- rbind.fill(df,temp)
      
      # determine the remaining number of pages for this search
      pages_left <- ceiling(results/100)-1
      
      # read in the remaining pages for this search
      for (page in 1:pages_left) {
        next_100 <- paste0("&firstRecord=",page*100)
        next_url <- paste0(url, next_100)
        
        page <- read_html(next_url)
        
        text <- page %>%
          html_nodes("script") %>%
          html_text()
        
        temp <- as.data.frame(text) %>%
          filter(grepl('type":"Car', text))
        df <- rbind.fill(df,temp)
        
      }
    }
  }
}

################## 3. Production Years: 14 - 40 Years Ago

# the URL layout for a date range is a little different
# URL Pattern
beg_url <- "https://www.autotrader.com/cars-for-sale/all-cars/richmond-va-23220?channel=ATC&dma=&searchRadius=50&startYear="
# earliest year
mid_url <- "&endYear="
# latest year
end_url <- "&isNewSearch=true&marketExtension=include&showAccelerateBanner=false&sortBy=relevance&numRecords=100"

# create year range for this section
fourteen_years_back <- year(Sys.Date())-14
earliest_year <- year(Sys.Date())-39
url <- paste0(beg_url, earliest_year, mid_url, fourteen_years_back, end_url)
# read URL html
page <- read_html(url)
# get number of listings on this page
results <- page %>%
  html_nodes("#mountNode > div:nth-child(1) > div.colored-background.inset.bg-gray-lightest.padding-top-4 > div > div.row.margin-horizontal-0.padding-horizontal-0 > div.row > div > div.display-flex.justify-content-between.align-items-center.justify-content-end.margin-bottom-2 > div.results-text-container.text-size-200") %>%
  html_text()

# if there are no results, end loop
if (results=="0 Results") {
  print(results)
  
  # if non-zero number of listings, add those on this page to the data frame
} else {
  print(results)
  
  # get number of results as a numeric value
  results <- strsplit(results, split = "of")[[1]][2]
  results <- strsplit(results, split = "Results")
  results <- as.numeric(gsub(" ","", results[[1]][1]))
  
  # if the number of results is 100 or less, read results and move to next search
  if (results<101) {
    text <- page %>%
      html_nodes("script") %>%
      html_text()
    
    temp <- as.data.frame(text) %>%
      filter(grepl('type":"Car', text))
    df <- rbind.fill(df,temp)
    
    # if results > 100, read this page, then move to next page until you've read in the last page of listings for this search
  } else {
    
    # read in the first page of this search
    text <- page %>%
      html_nodes("script") %>%
      html_text()
    
    temp <- as.data.frame(text) %>%
      filter(grepl('type":"Car', text))
    df <- rbind.fill(df,temp)
    
    # determine the remaining number of pages for this search
    pages_left <- ceiling(results/100)-1
    
    # read in the remaining pages for this search
    for (page in 1:pages_left) {
      next_100 <- paste0("&firstRecord=",page*100)
      next_url <- paste0(url, next_100)
      
      page <- read_html(next_url)
      
      text <- page %>%
        html_nodes("script") %>%
        html_text()
      
      temp <- as.data.frame(text) %>%
        filter(grepl('type":"Car', text))
      df <- rbind.fill(df,temp)
    }
  }
}

##############################
# Remove duplicates 
df <- distinct(df)

df_test <- df #%>%
 # filter(grepl(',","', text))

df_test$text <- gsub(',","','","',df_test$text)

# separate single string into multiple columns using comma as delimiter
x <- strsplit(df_test$text, split = ',"')

# create loop that makes a data frame out of each element of list x, rbind.fill each dataframe to df1
df_1 = data.frame()

for(i in 1:length(x)){
  temp <- data.frame(matrix(ncol = length(x[[i]]), nrow = 0))
  # colnames(temp) <- make.unique(sub('":.*',"",x[[i]]))
  colnames(temp) <- make.unique(gsub("\\@",'',sub('":.*',"",x[[i]])))
  temp[1, ] <- sub('.*":',"",x[[i]])
  df_1 <- rbind.fill(df_1,temp)
}
rm(temp)
rm(i)

# select useful columns and rename for clarity, clean data
richmond_listings <- df_1 %>%
  select(availability=availability,
         bodyType=bodyType,
         brand=name.2,
         color=color,
         description=description,
         driveWheelConfiguration=driveWheelConfiguration,
         fuelEfficiency=fuelEfficiency,
         fuelType=fuelType,
         full_listing_name=name,
         itemCondition=itemCondition,
         manufacturer=name.3,
         model=model,
         Odometer=value,
         price=price,
         priceCurrency=priceCurrency,
         priceValidUntil=priceValidUntil,
         seller_address_type=type.2,
         seller_image_url=image.1,
         seller_locality=addressLocality,
         seller_name=name.1,
         seller_phone=telephone,
         seller_postalCode=postalCode,
         seller_region=addressRegion,
         seller_street_address=streetAddress,
         seller_type=type.1,
         url=url,
         vehicle_image_url=image,
         vehicleEngine=vehicleEngine,
         vehicleInteriorColor=vehicleInteriorColor,
         vehicleTransmission=vehicleTransmission,
         vin=vehicleIdentificationNumber,
         productionYear=productionDate,
  ) %>%
  lapply(gsub, pattern='"|\\{|\\}|\\[|\\]', replacement='') %>%
  lapply(gsub, pattern='http://schema.org/', replacement='') %>%
  as.data.frame()

# write df to csv
write.csv(richmond_listings, file = paste0(Sys.Date(),"_","listings.csv"))
