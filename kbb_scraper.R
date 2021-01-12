# Clear environment
rm(list = ls())
# Clear console
cat("\014")

library(rvest)
library(tidyr)
library(dplyr)
library(data.table)
library(purrr)

setwd("/Users/thomasmcneill/Documents/data")

listings <- fread("2021-01-06_listings.csv", check.names = T)

## create new combined column for brand and model in same format as appears in kbb urls
listings$brand_model_url <- paste0(
  tolower(gsub(" ","-", listings$brand)),
  "/",
  tolower(gsub(" ","-", listings$model)),
  "/",
  tolower(gsub(" ","-", listings$productionYear)),
  "/")

## get distinct list of brands and models
brand_model_url <- listings %>%
  distinct(brand_model_url) %>%
  setorder(brand_model_url)

## create df for kbb ratings to fill

brand_model_year <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(brand_model_year) <- c("row","car")

expertr <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(expertr) <- c("row","expert_rating")

consumerr <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(consumerr) <- c("row","consumer_rating")

 for(i in 1:nrow(brand_model_url)){
#for(i in 1448:nrow(brand_model_url)){ ## stopped at 1447 on first attempt with no error - re-ran after and it worked without issue
    
  print(brand_model_url[i])
  
  ## create kbb for each brand and model
  temp <- paste0("https://www.kbb.com/", brand_model_url[i])
  
  ## read html of the page for each brand/model, if url doesnt exist, next
  page <- try(read_html(temp))
  
  if(inherits(page, "try-error"))
  {
    # error handling
    next
  }
  #rest of iteration for case of no error
  
  ## get the kbb rating for latest prod year of the model
  expert_rating <- page %>%
    html_nodes(xpath = '//*[@id="overview"]/div/div/div/div[1]/div/div/div[2]/div/div[3]/div/div/div[1]/div/div[1]/div/div/div[1]') %>%
    html_text()
  
  consumer_rating <- page %>%
    html_nodes(xpath = '//*[@id="overview"]/div/div/div/div[1]/div/div/div[2]/div/div[3]/div/div/div[1]/div/div[2]/div/div/div[1]') %>%
    html_text()

  ## add to the dataframe of ratings
  
  brand_model_year[i, ] <- c(i, brand_model_url[i]) # ,expert_rating, consumer_rating
  expertr[i, ] <- c(i, expert_rating)
  consumerr[i, ] <- c(i, consumer_rating)
  
  print(paste0("Expert: ", expert_rating," Consumer:", consumer_rating))
}

# if a rating is missing, the value for it will equal i. Makes these NA
er <- expertr %>%
  filter(!is.na(expert_rating)) %>%
  filter(row!=expert_rating)

cr <- consumerr %>%
  filter(!is.na(consumer_rating)) %>%
  filter(row!=consumer_rating)

# combine ratings into one df "ratings"
ratings <- merge(er, cr, by = "row", all = TRUE)

# match car to ratings
kbb <- merge(brand_model_year, ratings, by = "row", all = TRUE)

write.csv(kbb, file = "kbb_ratings.csv")

