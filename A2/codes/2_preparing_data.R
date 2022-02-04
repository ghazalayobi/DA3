
#############################
##     Data analysis 3     ##
##                         ##
##       Assignment II.    ##
##                         ##
##       Data cleaning     ##
##                         ##
##     Ghazal Ayobi        ##
#############################


# SET UP ------------------------------------------------------------------

# IN: data from web
# OUT: airbnb_ny.csv

#setting working directory
rm(list=ls())

library(tidyverse)
library(stargazer)
library(Hmisc)


getwd()

setwd("/Users/ghazalayobi/DA3/A2/")

path <- "/Users/ghazalayobi/DA3/A2/"

#location folders
data_in  <- paste0(path,"/data/clean/")
data_out <- paste0(path,"/data/clean/")
output <- paste0(path,"output/")


# set data dir, load theme and functions
source("https://raw.githubusercontent.com/ghazalayobi/DA3/main/da_helper_functions.R")
source("https://raw.githubusercontent.com/ghazalayobi/DA3/main/theme_bg.R") # theme_bg


options(digits = 3)

# Import data
df <- read_csv(paste(data_in,"airbnb_ny_workfile.csv", sep = ""))

# Research question applies to apartments for 2-6 guests

###
# Keep only properties with number of guests between 2-6
df <- df %>%  filter(df$accommodates >= 2 & df$accommodates <= 6)


# Keep if property type Apartment, Condominium or serviced apartment, entire loft, entire condo
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

df$property_type <- df$property_type %>% replace(df$property_type == 'Entire home/apt', "Apartment")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire serviced apartment', "Apartment")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire condominium (condo)', "Condominium")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire loft', "Loft")

df <- df %>% 
  mutate(f_property_type = factor(property_type))

unique(df$f_property_type)

#Room type as factor
df %>% 
  group_by(room_type) %>% 
  summarise(cnt = n())

df$room_type <- df$room_type %>% replace(df$room_type == 'Entire home/apt', "Entire/Apt")


# Factor room type
df <- df %>%
  mutate(f_room_type = factor(room_type))



# factor neighbourhood_grouped_cleansed
unique(df$neighbourhood_group_cleansed)

df <- df %>% 
  mutate(f_neighbourhood_group_cleansed = factor(neighbourhood_group_cleansed))


# factor neighborhood_cleansed
unique(df$neighbourhood_cleansed)
df <- df %>% mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed))



# get host_response_time as factors

df %>% 
  group_by(host_response_time) %>% 
  summarise(cnt = n())

df$host_response_time[is.na(df$host_response_time)]<- "N/A"
unique(df$host_response_time)

df <- df %>% 
  mutate(f_host_response_time = factor(host_response_time))



#--------------------------------------------------------------------------

## Create Numerical variables
df <- as.data.frame(df)
df$price <- as.numeric(df$price)

library(dplyr)

# clean number of bathrooms
unique(df$bathrooms)
df <- df[!df$bathrooms_text == "null", ] 
df <- df %>% rename(bathrooms = bathrooms_text)
# get the number of baths from bathroom_text

df$bathrooms <- df$bathrooms %>% replace(df$bathrooms == 'Half-bath', "0.5")
df$bathrooms <- gsub("baths","",df$bathrooms)
df$bathrooms <- gsub("bath","",df$bathrooms)
unique(df$bathrooms)



#add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms","review_scores_rating","number_of_reviews","minimum_nights","beds", "availability_365")
df <- df %>% mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- df %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(df))
colnames(df)[nnames_i] <- paste0("n_", numericals)

# create days since first review
df <- df %>% mutate(n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                                    as.Date(first_review ,format="%Y-%m-%d")))


# create dummy vars
colnames(df)

dummies <- df[, 50:161]

df <- df %>% mutate_at(vars(dummies), funs("d"= (.)))

# rename columns
dnames <- df %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(df))
colnames(df)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

# keep columns if contain d_, n_,f_, p_, usd_ and some others
df <- df %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed,room_type,property_type)

# Drop observations lacking price
df <- df %>% drop_na(price)


write_csv(df, paste0(data_out, "airbnb_ny_workfile_v2.csv"))


#-------------------------------------------------------------------------------

# Data Exploration

summary(data$price)
describe(data$price)

# Remove extreme values
data <- data %>%
  filter(price <= 500)

# Histograms
price_distribution <- ggplot(data, aes(price)) +
  geom_histogram(binwidth = 25, fill = "indianred3", color = "black") +
  ylab("Count") +
  xlab("Price") +
  theme_bw()
price_distribution

# Take log of price
data <- data %>% mutate(ln_price = log(price))

log_price_distribution <- ggplot(data, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, fill = "indianred3", color = "black") +
  ylab("Count") +
  xlab("Log price") +
  theme_bw()
log_price_distribution





