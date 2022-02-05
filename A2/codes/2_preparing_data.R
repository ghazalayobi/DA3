
#############################
##     Data analysis 3     ##
##                         ##
##       Assignment II.    ##
##                         ##
##       Data Preparing    ##
##                         ##
##       Ghazal Ayobi      ##
#############################


# Begin ########################################################################

# IN: data from web
# OUT: airbnb_ny.csv

#setting working directory
rm(list=ls())

library(tidyverse)
library(stargazer)
library(viridis)
library(modelsummary)
library(Hmisc)


getwd()

setwd("/Users/ghazalayobi/DA3/A2")

path <- "/Users/ghazalayobi/DA3/A2"

#location folders
data_in  <- paste0(path,"/data/clean/")
data_out <- paste0(path,"/data/clean/")
output <- paste0(path,"output/")


# load theme and functions
source("https://raw.githubusercontent.com/ghazalayobi/DA3/main/da_helper_functions.R")
source("https://raw.githubusercontent.com/ghazalayobi/DA3/main/theme_bg.R") # theme_bg

# Import data
df <- read_csv(paste(data_in,"airbnb_ny_workfile_v1.csv", sep = ""))

# The main objective is to filter for apartments for 2-6 guests
df <- df %>%  filter(df$accommodates >= 2 & df$accommodates <= 6)


# Keeping if property type is Apartment, Condominium or serviced apartment, entire loft, entire condo
unique(df$property_type)

df <- df %>% filter(property_type %in% c("Apartment", "Condominium", "Serviced apartment", 
                                             "Entire loft", "Entire condominium (condo)",
                                             "Entire serviced apartment", "Entire home/apt"
                                             ))


# checking for property type
df %>% 
  group_by(property_type) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

# grouping the property types
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire home/apt', "Apartment")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire serviced apartment', "Apartment")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire condominium (condo)', "Condominium")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire loft', "Loft")

# factoring property types
df <- df %>% 
  mutate(f_property_type = factor(property_type))

# checking the factored column
unique(df$f_property_type)

#-----
# Factoring room type

# checking the sub categories of room types
df %>% 
  group_by(room_type) %>% 
  summarise(cnt = n())

# in the selected data set there is only one room type
df$room_type <- df$room_type %>% replace(df$room_type == 'Entire home/apt', "Entire/Apt")

# Create new variable for Factor room type 
df <- df %>%
  mutate(f_room_type = factor(room_type))

#-----

# Creating  host response time factors

# checking for summary
df %>% 
  group_by(host_response_time) %>% 
  summarise(cnt = n())


df <- df[!df$host_response_time == "N/A", ]

df <- df %>%
  drop_na(host_response_time)

df %>% 
  group_by(host_response_time) %>% 
  summarise(cnt = n())

# Creating factored variable for host response time
df <- df %>% 
  mutate(f_host_response_time = factor(host_response_time))

#-----
# factoring neighborhood_grouped_cleansed column 
unique(df$neighbourhood_group_cleansed) # checking the sub categories

# factoring the column for grouped neighborhood
df <- df %>% 
  mutate(f_neighbourhood_group_cleansed = factor(neighbourhood_group_cleansed))

#-----
# Create another factor variable for factor neighborhood_cleansed, this column is detailed description of neighborhoods
unique(df$neighbourhood_cleansed)
df <- df %>% mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed))


################################################################################

## Create Numerical variables
df <- as.data.frame(df)
df$price <- as.numeric(df$price)


# cleaning the bathrooms_text column
unique(df$bathrooms) # checking for all values

df <- df[!df$bathrooms_text == "null", ] 
df <- df %>% rename(bathrooms = bathrooms_text)

# get the number of baths from bathroom_text
df$bathrooms <- df$bathrooms %>% replace(df$bathrooms == 'Half-bath', "0.5")
df$bathrooms <- gsub("baths","",df$bathrooms)
df$bathrooms <- gsub("bath","",df$bathrooms)
unique(df$bathrooms) # checking the values
df$bathrooms <- as.numeric(df$bathrooms) # transforming to numerics

# Adding new numeric columns from the numeric variables

numerics <- c("accommodates","bathrooms", "reviews_per_month", 
                "review_scores_rating","number_of_reviews","minimum_nights", 
                "beds", "availability_365", "bedrooms")
df <- df %>% mutate_at(vars(numerics), funs("n"=as.numeric))


# Creating _n column name for the numerics
numerics_names <- df %>%
  select(ends_with("_n")) %>%
  names()
numerics_names_i <- match(numerics_names, colnames(df))
colnames(df)[numerics_names_i] <- paste0("n_", numerics)

################################################################################

# Adding new dummy variables from data

data_dummies <- c("host_is_superhost", "host_identity_verified", "instant_bookable", "has_availability" )
df <- df %>% mutate_at(vars(data_dummies), funs("d"=as.numeric))


# Creating _d column name for the dummies
binary_names <- df %>%
  select(ends_with("_d")) %>%
  names()
binary_names_i <- match(binary_names, colnames(df))
colnames(df)[binary_names_i] <- paste0("d_", data_dummies)



################################################################################

# Creating dummy variables
colnames(df)

# creating new data table for dummy variables
dummies <- df[, 52:161]
dummies <- colnames(dummies)

df <- df %>% mutate_at(vars(dummies), funs("d"= (.)))

# rename columns
dnames <- df %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(df))
colnames(df)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))


# keeping columns if contain d_, n_,f_, p_, usd_ and some others
df <- df %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id, neighbourhood_group_cleansed,
         neighbourhood_cleansed, room_type, property_type, latitude, longitude)

# Drop null values
df <- df %>% drop_na(price)
df <- df %>% drop_na(property_type)
df <- df %>% drop_na(room_type)
df <- df %>% drop_na(neighbourhood_cleansed)
df <- df %>% drop_na(neighbourhood_group_cleansed)
df <- drop_na(df)



################################################################################


# Data Exploration

# Price
#################


summary(df$price)

# Removing extreme values
df <- df %>%
  filter(price <= 450)

# Price Histogram

hist_price <- ggplot(data=df, aes(x=price)) +
  geom_histogram(binwidth = 15, boundary=0, fill = "#440154", 
                 color = "white", size = 0.2, alpha = 0.5,  show.legend=F, na.rm=TRUE) +
  labs(x = "Price",y = "Count")+
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  ggtitle("Price Histogram") +
  theme(panel.border = element_blank(), axis.text=element_text(size=8), 
        plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))
hist_price


# Taking log of price

df <- df %>% mutate(ln_price = log(price))

hist_lnprice <- ggplot(data=df, aes(x=ln_price)) +
  geom_histogram(boundary=0, fill = "#440154", color = "white", 
                 size = 0.2, alpha = 0.5,  show.legend=F, na.rm=TRUE) +
  labs(x = "ln Price",y = "Count")+
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  ggtitle("ln Price") +
  theme(panel.border = element_blank(), axis.text=element_text(size=8), 
        plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))
hist_lnprice


# Accommodates
#################

sum_accomm <- datasummary(price* (`Accommodates` = as.factor(n_accommodates)) ~ N + min + max + 
              Percent() + mean, data = df, title = "Accommodates Summary")


sum_accomm

loess_accomm <- ggplot( df , aes(x = n_accommodates, y = price)) +
  geom_point(size=0.5,alpha=0.6, na.rm = T, color = "#440154") +
  geom_smooth(method="loess" , formula = y ~ x , na.rm = T, color = "#fde725" )+
  theme_bw() +
  scale_color_viridis(option = "D", discrete = TRUE) +
  scale_fill_viridis(option = "D", discrete = TRUE) +
  labs(x = "Accommodates",y = "Price in USD") +
  theme(panel.border = element_blank(), axis.text=element_text(size=8), 
        plot.title = element_text(size = 12L, face = "bold", hjust = 0.5)) +
  ggtitle("Loess for Accommodates")


loess_accomm

# Bathrooms
#################

sum_baths <- datasummary(price* (`Bathrooms` = as.factor(n_bathrooms)) ~ N + min + max + 
                            Percent() + mean, data = df, title = "Bathrooms Summary")

sum_baths

# More than 80% of observations has one bathroom
# creating pool variables

unique(df$n_bathrooms)
df$n_bathrooms <- round(df$n_bathrooms)

df <- df %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0, 1, 2, 3), labels=c(0,1,2), right = F) )

unique(df$n_bathrooms)
# Beds
#################
sum_beds <- datasummary(price* (`Beds` = as.factor(n_beds)) ~ N + min + max + 
                           Percent() + mean, data = df, title = "Beds Summary")

sum_beds

loess_beds <- ggplot( df , aes(x = n_beds, y = price)) +
  geom_point(size=0.5,alpha=0.6, na.rm = T, color = "#440154") +
  geom_smooth(method="loess" , formula = y ~ x , na.rm = T, color = "#fde725" )+
  theme_bw() +
  scale_color_viridis(option = "D", discrete = TRUE) +
  scale_fill_viridis(option = "D", discrete = TRUE) +
  labs(x = "Bed",y = "Price in USD") +
  theme(panel.border = element_blank(), axis.text=element_text(size=8), 
        plot.title = element_text(size = 12L, face = "bold", hjust = 0.5)) +
  ggtitle("Loess for Bed")


loess_beds

# there are very few observations for 5 or 6 beds

# Bedroom
#################
sum_bedroom <- datasummary(price* (`Beds` = as.factor(n_bedrooms)) ~ N + min + max + 
                          Percent() + mean, data = df, title = "Bedrooms Summary")


sum_bedroom
# 65% of observations has one bedroom

# Number of reviews
#####################

# checking summary for number of reviews
sum_n_reviews <- datasummary((`Reviews` = n_number_of_reviews) ~ N + min + 
                               max + mean, data = df, title = "Number of Reviews Summary")

sum_n_reviews

# checking the distribution of number of reviews

hist_n_reviews <- ggplot(data=df, aes(x=n_number_of_reviews)) +
  geom_histogram(binwidth = 10, boundary=0, fill = "#440154", color = "white", 
                 size = 0.2, alpha = 0.5,  show.legend=F, na.rm=TRUE) +
  labs(x = "Number of Reviews",y = "Count")+
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  ggtitle("Number of Reviews") +
  theme(panel.border = element_blank(), axis.text=element_text(size=8), 
        plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))
hist_n_reviews


# checking relationship between number of reviews and price 
loess_reviews <- ggplot( df , aes(x = n_number_of_reviews, y = price)) +
  geom_point(size=0.5,alpha=0.6, na.rm = T, color = "#440154") +
  geom_smooth(method="loess" , formula = y ~ x , na.rm = T, color = "#fde725" )+
  theme_bw() +
  scale_color_viridis(option = "D", discrete = TRUE) +
  scale_fill_viridis(option = "D", discrete = TRUE) +
  labs(x = "Review score rating",y = "Price in USD") +
  theme(panel.border = element_blank(), axis.text=element_text(size=8), 
        plot.title = element_text(size = 12L, face = "bold", hjust = 0.5)) +
  ggtitle("Loess for Number of Review")


loess_reviews

# Pool number of reviews for these cuts : 0,1, 2, 3, 10, 20, 50 and above 50
df <- df %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1, 2, 3, 10, 20, 50,max(df$n_number_of_reviews)), labels=c(0,1, 2, 3, 10, 20, 50), right = F))


# Number of review score rating
#######################

# checking summary for review score rating
sum_review_scores <- datasummary((`Reviews score` = n_review_scores_rating) ~ N + min + 
                               max + mean, data = df, title = "Number of Review Scores Rating Summary")

sum_review_scores

# checking relationship between number of review scores and price
loess_review_scores <- ggplot( df , aes(x = n_review_scores_rating, y = price)) +
  geom_point(size=0.5,alpha=0.6, na.rm = T, color = "#440154") +
  geom_smooth(method="loess" , formula = y ~ x , na.rm = T, color = "#fde725" )+
  theme_bw() +
  scale_color_viridis(option = "D", discrete = TRUE) +
  scale_fill_viridis(option = "D", discrete = TRUE) +
  labs(x = "Review score rating",y = "Price in USD") +
  theme(panel.border = element_blank(), axis.text=element_text(size=8), 
        plot.title = element_text(size = 12L, face = "bold", hjust = 0.5)) +
  ggtitle("Loess for Review Score Rating")


loess_review_scores



# minimum nights
#######################

# checking summary of minimum number of nights
sum_nnights <- datasummary((`Reviews score` = n_minimum_nights) ~ N + min + 
                                   max + mean, data = df, title = "Minimum nights Summary")
sum_nnights

# checking relationship between number of minimum nights and price
loess_nnights <- ggplot( df, aes(x = n_minimum_nights, y = price)) +
  geom_point(size=0.5,alpha=0.6, na.rm = T, color = "#440154") +
  geom_smooth(method="loess" , formula = y ~ x , na.rm = T, color = "#fde725" )+
  theme_bw() +
  scale_color_viridis(option = "D", discrete = TRUE) +
  scale_fill_viridis(option = "D", discrete = TRUE) +
  labs(x = "Minimum nights",y = "Price in USD") +
  theme(panel.border = element_blank(), axis.text=element_text(size=8), 
        plot.title = element_text(size = 12L, face = "bold", hjust = 0.5)) +
  ggtitle("Loess for Minimum nights")


loess_nnights

# Keeping observations for only one and half month or 45 nights
df <- df[df$n_minimum_nights <= 45, ]


################################################################################
# Factor Variables

# Room Type
#######################

sum_room_type <- datasummary(price* (`Room Type` = as.factor(f_room_type)) ~ N + min + max + 
                           Percent() + mean, data = df, title = "Room Type Summary")
sum_room_type

# Property Type
#######################

sum_property_type <- datasummary(price* (`Property Type` = as.factor(f_property_type)) ~ N + min + max + 
                               Percent() + mean, data = df, title = "Property Type Summary")
sum_property_type

# Host Response time
#######################

sum_host_res_time <- datasummary(price* (`Room Type` = as.factor(f_host_response_time)) ~ N + min + max + 
                               Percent() + mean, data = df, title = "Host Response Time Summary")
sum_host_res_time



################################################################################
# Deal with missing values                     

# creating filter function
to_filter <- sort(sapply(df, function(x) sum(is.na(x)/nrow(df)*100)))
to_filter[to_filter > 0]

# zero found


# Replace missing variables with flags
df <- df %>%
  mutate(f_number_of_reviews =  ifelse(is.na(n_rnumber_of_reviews), mean(n_number_of_reviews, na.rm = T), n_number_of_reviews)
  )

# checking again for missing variables
to_filter <- sort(sapply(df, function(x) sum(is.na(x)/nrow(df)*100)))
to_filter[to_filter > 0]
# zero found



# save workfile
write.csv(df, paste0(data_out, "airbnb_ny_cleaned.csv"))




