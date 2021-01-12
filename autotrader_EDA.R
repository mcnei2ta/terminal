# Clear environment
rm(list = ls())
# Clear console
cat("\014")

library(tidyr) ## spread
library(dplyr) ## mutate, filter, select, distinct, group_by, summarize
library(data.table) ## fread, merge, rbind, setorder
library(ggplot2) ## for plots
library(cowplot) ## print plots side by side

setwd("/Users/thomasmcneill/Documents/data")

# read in autotrader data - within 50 miles of my zip code
listings <- fread("autotrader_listings.csv", check.names = T)

# add kbb ratings to each car from separate kbb webscrape
#### create new combined column for brand and model in same format as appears in kbb urls (to join)
listings$car <- paste0(
  tolower(gsub(" ","-", listings$brand)),
  "/",
  tolower(gsub(" ","-", listings$model)),
  "/",
  tolower(gsub(" ","-", listings$productionYear)),
  "/")

#### read in kbb data
kbb <- fread("kbb_ratings.csv", check.names = T)

#### merge dataframes (same as a left join from listings to kbb)
listings <- merge(listings, kbb, by = "car", all.x = TRUE)

# format and select columns to keep
listings$Odometer <- as.numeric(gsub(",","", listings$Odometer))
listings <- listings %>%
  select(manufacturer, model, productionYear, bodyType, price, Odometer, color, expert_rating, 
         consumer_rating, url, vehicle_image_url, itemCondition, driveWheelConfiguration, vehicleInteriorColor, 
         fuelType, fuelEfficiency, vehicleEngine, vehicleTransmission, seller_name, vin) %>%
  setorder(-expert_rating, price)

glimpse(listings)

###### Pivots

car_year <- listings %>%
  group_by(productionYear) %>%
  summarise(num_listings=n()) %>%
  setorder(-productionYear)

car_count <- listings %>%
  filter(productionYear>2009) %>%
  group_by(manufacturer) %>%
  summarise(num_listings=n()) %>%
  filter(num_listings>2) %>%
  setorder(-num_listings)

car_price <- listings %>%
  filter(price>0) %>%
  filter(productionYear>2009) %>%
  group_by(manufacturer) %>%
  summarise(avg_price=mean(price)) %>%
  setorder(-avg_price)

car_exp_rating <- listings %>%
  filter(productionYear>2009) %>%
  filter(!is.na(expert_rating)) %>%
  group_by(manufacturer) %>%
  summarise(avg_rating=mean(expert_rating)) %>%
  setorder(-avg_rating)

car_cons_rating <- listings %>%
  filter(productionYear>2009) %>%
  filter(!is.na(consumer_rating)) %>%
  group_by(manufacturer) %>%
  summarise(avg_rating=mean(consumer_rating)) %>%
  setorder(-avg_rating)

car_type <- listings %>%
  filter(!is.na(bodyType)) %>%
  filter(bodyType != "Unavailable ") %>%
  group_by(bodyType) %>%
  summarise(num_listings=n()) %>%
  setorder(-num_listings)

##### Graphs

##### Distribution of prices
listings %>%
  filter(price>0) %>%
  filter(price<100000) %>%
  ggplot(aes(price/1000)) +
  geom_histogram(bins=100)

##### counts and prices by manufacturer 
### manufacturer by number of listings
plot_count <- car_count %>%
  ggplot(aes(x=reorder(manufacturer, num_listings))) +
  geom_col(aes(y=num_listings)) +
  coord_flip()
plot_count

### manufacturer by average price
plot_price <- car_price %>%
  ggplot(aes(x=reorder(manufacturer, avg_price))) +
  geom_col(aes(y=avg_price)) + 
  coord_flip()

plot_grid(plot_count, plot_price)

####### ratings by manufacturer
### manufacturer by average expert rating
plot_exp_rating <- car_exp_rating %>%
  ggplot(aes(x=reorder(manufacturer, avg_rating))) +
  geom_col(aes(y=avg_rating)) + 
  coord_flip()

plot_cons_rating <- car_cons_rating %>%
  ggplot(aes(x=reorder(manufacturer, avg_rating))) +
  geom_col(aes(y=avg_rating)) + 
  coord_flip()

plot_grid(plot_exp_rating, plot_cons_rating)

####### year and type by manufacturer 
### listings by body type
plot_type <- car_type %>%
  ggplot(aes(x=reorder(bodyType, num_listings))) +
  geom_col(aes(y=num_listings)) + 
  coord_flip()

### listings by production year
plot_year <- car_year %>%
  filter(productionYear>1999) %>%
  ggplot(aes(x=productionYear)) +
  geom_col(aes(y=num_listings)) + 
  coord_flip()

plot_grid(plot_type, plot_year)


##### Custom Search
custom_search <- listings %>%
  filter(price>0) %>%
  filter(price<13000) %>%
  filter(productionYear>2010) %>%
  filter(productionYear<2019) %>%
  filter(expert_rating>4.2)%>%
  filter(Odometer<130000) %>%
  setorder(manufacturer, model, price, Odometer)
#   filter(bodyType %in% c("Truck"))
#  filter(manufacturer %in% c("Lamborghini","Aston Martin","Bentley")) %>%
#  filter(model %in% c("Dart")) %>%
  # group_by(manufacturer, model, bodyType) %>%  #productionYear, 
  # summarise(Total=n(), avgOdom=mean(Odometer), avgPrice=mean(price), minPrice=min(price), maxPrice=max(price)) %>%
  # setorder(manufacturer, avgPrice, model)

# regression
fit <- lm(price ~ productionYear + bodyType + Odometer + expert_rating + manufacturer, data=listings)
summary(fit)

write.csv(custom_search, file = "Listings_Custom_Search.csv")

#### To Do
# labeling for ggplot2
# make R notebook
