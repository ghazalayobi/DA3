##     Data analysis 3     ##
##                         ##
##       Assignment II.    ##
##                         ##
##       Data cleaning     ##
##        & Preparing      ##
##                         ##
##      Ghazal Ayobi       ##


# IN: data from web
# OUT: airbnb_ny.csv

#setting working directory
rm(list=ls())

library(tidyverse)
library(fastDummies)
library(scales)
library(viridis)
library(modelsummary)
library(Hmisc)
library(data.table)



source("https://raw.githubusercontent.com/ghazalayobi/DA3/main/da_helper_functions.R")
source("https://raw.githubusercontent.com/ghazalayobi/DA3/main/theme_bg.R")


getwd()



setwd("/Users/ghazalayobi/DA3/A2")

path <- "/Users/ghazalayobi/DA3/A2"

#location folders
data_in  <- paste0(path,"/data/raw/")
data_out <- paste0(path,"/data/clean/")
# Reading the data
df <- read.csv(paste0(data_in,"listings.csv"), fileEncoding="UTF-8")


# Droping unnecessary columns ##################################################
names(df) # to check list of all variables

drops <- c("name",
           "listing_url", 
           "last_scraped", 
           "description",
           "host_thumbnail_url",
           "neighborhood_overview",
           "host_picture_url",
           "thumbnail_url",
           "medium_url",
           "picture_url",
           "xl_picture_url",
           "host_url",
           "experiences_offered", 
           "notes", 
           "transit", 
           "access", 
           "interaction", 
           "house_rules", 
           "host_about", 
           "summary", 
           "space", 
           "host_location",
           "host_total_listings_count",
           "bathrooms", # Contains NA values
           "minimum_minimum_nights","maximum_maximum_nights","minimum_maximum_nights",
           "maximum_minimum_nights","minimum_nights_avg_ntm","maximum_nights_avg_ntm", 
           "number_of_reviews_ltm", "is_business_travel_ready", 
           "calculated_host_listings_count_entire_homes", 
           "calculated_host_listings_count_private_rooms", 
           "calculated_host_listings_count_shared_rooms",
           "calendar_updated", "review_scores_accuracy",
           "review_scores_checkin", "review_scores_location",
           "review_scores_value", "review_scores_communication"
)


df <- df[ , !(names(df) %in% drops)]

# Formatting columns
# remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

#remove dollar signs from price variables
for (i in 1:nrow(df)){
  df$price[i] <- as.numeric(gsub("\\$","",as.character(df$price[i])))
}

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified", "has_availability", "instant_bookable")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}


df$price <- as.numeric(df$price)
df$amenities <- as.character(df$amenities)

#Amenities----------------------------------------------------------------------
# Cleaning amenities

df$amenities <- gsub("\\[","",df$amenities)
df$amenities <- gsub("\\]","",df$amenities)
df$amenities <- gsub('\\"',"",df$amenities)
df$amenities <- gsub('2013',"", as.character(df$amenities)) #removing \u2013
df$amenities <- gsub('2014',"", as.character(df$amenities)) #removing \u2014
df$amenities <- gsub('2019s',"", as.character(df$amenities)) #removing \u2019s
df$amenities <- gsub('2019',"", as.character(df$amenities)) # removing \u2019
df$amenities <- gsub('\\\\u',"", as.character(df$amenities)) # removing \u
df$amenities <- as.list(strsplit(df$amenities, ","))

#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))
df<-cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
df <- df[ , !(names(df) %in% drops)] # droping the list of drops
df <- df[ , !(names(df) %in% "license")] # droping the license because it has no values


# create data frame of the amenities
amts <- df %>% select(-(1:43))


# deleting spaces in the beginning and at the end of the column, and transfer all to lower case
names(amts) <- gsub(" ","_", tolower(trimws(names(amts))))
names(df) <- tolower(names(df))


# look at the column names we have
levs <- sort(names(amts))


# list of key words Key words
column_names <- c( "kitchen", "stove|kitchenaid_stainless_steel_gas_stove", "oven|kitchenaid_stainless_steel_oven", 
                   "refrigerator|fridge|kitchen_aid_refrigerator|mini_fridge",
                   "coffee|nespresso_machine|keurig_coffee_machine", "grill",
                   "microwave","wifi|internet|ethernet_connection", "tv|cable_tv|hdtv|tv_with_standard_cable", 
                   "sound","toiletries", "shampoo|conditioner", "gel|soap","hair_dryer", 
                   "washer", "dryer", "iron", "hot_water","heating|heated_floor|fireplace|central_heating", 
                   "air_conditioning|fan|ac|central_air_conditioning","breakfast", "fitness|gym","children|baby|crib", 
                   "smoking", "balcony|terrace", 
                   "long_term_stays_allowed", "free.*on premises|free_street_parking", 
                   "paid.*off premises|self-parking", "clothing_storage|clothing_storage.*", 
                   "smoke_alarm", "housekeeping", "restaurant")



# function to merge columns with the same key word in them
for (i in column_names) {
  xdf <- amts %>% select(matches(i))
  
  amts$new_col <- ifelse(rowSums(xdf)>0, 1, 0)
  
  names(amts)[names(amts) == "new_col"] <- paste0("have_", i)
  
  amts <- amts %>% select(-colnames(xdf)) 
  
} 


# keep only columns where the percentage of 1s is at least 1% and at most 99%
selected <- sapply(names(amts), function(x){
  ratio <- sum(amts[[x]])/nrow(amts)*100
  if (between(ratio, 1, 99)) {
    return(TRUE)
  } else { return(FALSE) }
})

# taking only selected or grouped columns
amenities <- amts[,selected]

# checking the columns
colnames(amenities)

# The columns appear to have two values for the same same variables. 
#Thus, I decided to run the function run the function to merger all the columns with the same name
new_name <- names(amenities) # checking names of columns

# merge all the columns with the same column name
for (i in new_name) {
  xxdf <- amenities %>% select(matches(i))
  
  amenities$new_col <- ifelse(rowSums(xxdf)>0, 1, 0)
  
  names(amenities)[names(amenities) == "new_col"] <- paste0(i)
  
  amenities <- amenities %>% select(-colnames(xxdf)) 
  
} 

# keep only columns where the percentage of 1s is at least 1% and at most 99%
selected <- sapply(names(amenities), function(x){
  ratio <- sum(amenities[[x]])/nrow(amenities)*100
  if (between(ratio, 1, 99)) {
    return(TRUE)
  } else { return(FALSE) }
})

amenities <- amenities[,selected]


# selecting the first columns
df <- df %>% select((1:43))

# combining both data frames
df <- cbind(df, amenities)

# removing unnecessary values
rm(amts, amenities, binary, column_names, drops, i, levs, perc, selected, xxdf)

#------------------------------------------------------------------------------

# Filtering the data for the main question
# The main objective is to filter for apartments for 2-6 guests
df <- df %>%  filter(df$accommodates >= 2 & df$accommodates <= 6)


# selecting the property types: keeping only apartments
df <- df %>% filter(property_type %in% c("Apartment", "Condominium", "Serviced apartment", 
                                         "Entire loft", "Entire condominium (condo)",
                                         "Entire serviced apartment", "Entire home/apt",
                                         "Entire rental unit"))


# checking for property type
df %>% 
  group_by(property_type) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

# Renaming the property types
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire home/apt', "Apartment")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire serviced apartment', "Apartment")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire condominium (condo)', "Condominium")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire loft', "Loft")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire rental unit', "Entire Unit")


# factoring property types
df <- df %>% 
  mutate(f_property_type = factor(property_type))

#-------------------------------------------------
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

# factoring the column for grouped neighborhood
df <- df %>% 
  mutate(f_neighbourhood_group_cleansed = factor(neighbourhood_group_cleansed))
#factor neighborhood_cleansed
df <- df %>% mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed))

# droping NAs
df <- df %>% 
  drop_na(host_response_time)

df <- df %>% 
  mutate(f_host_response_time = factor(host_response_time, levels = c( "within an hour",  "within a few hours",
                                                                       "within a day", "a few days or more", "N/A")))
df <- df %>% 
  drop_na(f_host_response_time)

# where do we have missing variables?
to_filter <- sort(sapply(df, function(x) sum(is.na(x)/nrow(df)*100)))
to_filter[to_filter > 0]

#-----------------------------------------------
# Numeric Variables

df$price <- as.numeric(df$price)
describe(df$price)

# Create Numerical variables
# imputing 1 percent to the missing host response rate

df <- df %>%
  mutate(p_host_response_rate = ifelse(is.na(host_response_rate),1, host_response_rate ))
df$p_host_response_rate <- df$p_host_response_rate %>% replace(df$p_host_response_rate == "N/A", 1)

df <- df %>%
  mutate( p_host_response_rate = as.numeric(p_host_response_rate))


# Clean bathroom_text

df <- df[!df$bathrooms_text == "null", ] 
df <- df %>% rename(bathrooms = bathrooms_text)

# get the number of baths from bathroom_text
df$bathrooms <- df$bathrooms %>% replace(df$bathrooms == 'Half-bath', "0.5")
df$bathrooms <- gsub("baths","",df$bathrooms) # removing baths 
df$bathrooms <- gsub("bath","",df$bathrooms) # removing bath


# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms", "bedrooms", "beds", "review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights", "availability_365")
df <- df %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- df %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(df))
colnames(df)[nnames_i] <- paste0("n_", numericals)

#----------------------------------------

#Checking first review
df %>% 
  group_by(first_review) %>% 
  summarise(cnt = n())


#create days since first review
df <- df %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))


#---------------------------------------
# Dummies

colnames(df)
# create dummy vars
dummies <- c(names(df)[seq(44,133)],"host_is_superhost", "host_identity_verified", "host_has_profile_pic" )
df <- df %>%
  mutate_at(vars(dummies), funs("d"= (.)))

# rename columns
dnames <- df %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(df))
colnames(df)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))


# keep columns if contain d_, n_, f_, p_, usd_ and some others
df <- df %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed, neighbourhood_group_cleansed, room_type, property_type, latitude, longitude)

# with price info only
df <- df %>%
  drop_na(price)


#-------------------------------------------------------------------------------
# Cleaning values

summary(df$price)
describe(df$price)

# filter out really high extreme values
df <- df %>%
  filter(price <500)

# create ln price
df <- df %>%
  mutate(ln_price = log(price))


# Price Histogram
hist_price <- ggplot(data=df, aes(x=price)) +
  geom_histogram(binwidth = 15, fill = "#440154", color = "white", size = 0.25, alpha = 0.5,  show.legend=F, na.rm=TRUE) +
  labs(x = "Price",y = "Count")+
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  ggtitle("Price Histogram") +
  theme(panel.border = element_blank(), axis.text=element_text(size=8), 
        plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))
hist_price

# log of price
hist_lnprice <- ggplot(data=df, aes(x=ln_price)) +
  geom_histogram(binwidth = 0.15, boundary=0, fill = "#440154", color = "white", 
                 size = 0.2, alpha = 0.5,  show.legend=F, na.rm=TRUE) +
  labs(x = "ln Price",y = "Count")+
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  ggtitle("ln Price") +
  theme(panel.border = element_blank(), axis.text=element_text(size=8), 
        plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))
hist_lnprice

#----------------------------------------------
# Look at some numeric key vars                
#----------------------------------------------

# n_accommodates
# summary of accommodates
sum_accomm <- datasummary(price* (`Accommodates` = as.factor(n_accommodates)) ~ N + min + max + 
                            Percent() + mean, data = df, title = "Accommodates Summary")

sum_accomm
# accommodates lm distribution
lm_accomm <- ggplot( df , aes(x = n_accommodates, y = price)) +
  geom_point(size=1, alpha=0.6, na.rm = T, color = "#440154") +
  geom_smooth(method="lm" , formula = y ~ x , na.rm = T, color = "#fde725", se = FALSE )+
  theme_bw() +
  scale_color_viridis(option = "D", discrete = TRUE) +
  scale_fill_viridis(option = "D", discrete = TRUE) +
  labs(x = "Number of People Accommodated",y = "Price in USD") +
  theme(panel.border = element_blank(), axis.text=element_text(size=8), 
        plot.title = element_text(size = 12L, face = "bold", hjust = 0.5)) +
  ggtitle("Number of People Accommodated")
lm_accomm

#-------------------------------------------

# Beds
df %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# maybe best is to have log beds
df <- df %>%
  mutate(ln_beds = log(n_beds))

ggplot(df, aes(n_beds)) +
  geom_histogram( fill = "#440154", color = "white", alpha = 0.6, size = 0.25) +
  xlab("Nr beds") +
  theme_bw()

#-------------------------------------------
# n_bathrooms
ggplot(df, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = "#440154", color = "white", alpha = 0.6) +
  xlab("Nr bathrooms") +
  theme_bw()

df %>%
  group_by(n_bathrooms) %>%
  summarise(mean_price = mean(price), n = n())

# Pool accomodations with 0,1,2,10 bathrooms
df <- df %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,10), labels=c(0,1,2), right = F) )

df %>%
  group_by(f_bathroom) %>%
  summarise(mean_price = mean(price), n = n())



#-----------------------------------------
# n_bedrooms
ggplot(df, aes(n_bedrooms)) +
  geom_histogram(binwidth = 0.5, fill = "#440154", color = "white", alpha = 0.6, size = 0.25) +
  xlab("Nr bedrooms") +
  theme_bw()
# Bedrooms summary
sum_bedrooms <- datasummary(price* (`Bedrooms` = as.factor(n_bedrooms)) ~ N + min + max + 
                          Percent() + mean, data = df, title = "Bedrooms Summary")
sum_bedrooms
# Bedrooms summary and accommodates
sum_bedrooms2 <- datasummary((`Accommodates` = n_accommodates)* (`Bedrooms` = as.factor(n_bedrooms)) ~ N + min + max + 
                          Percent() + mean, data = df, title = "Bedrooms Summary")

sum_bedrooms2

#-------------------------------------
# Number of reviews
nreview_plot <- df %>%
  filter(n_number_of_reviews <100)

ggplot(nreview_plot, aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = "#440154", color = "white", alpha = 0.6, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_bw()

# number of reviews: use logs as well
df <- df %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))

ggplot(df, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, fill = "#440154", color = "white", alpha = 0.6, size = 0.25) +
  ylab("") +
  xlab("Log N of reviews") +
  theme_bw()

# Pool num of reviews to 3 categories: none, 1-51 and >51
df <- df %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(df$n_number_of_reviews)), labels=c(0,1,2), right = F))

# plotting new reviews
df %>%
  group_by(f_number_of_reviews) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())



#-------------------------------
# n_minimum_nights
# checking summary of minimum number of nights
sum_nnights <- datasummary((`Reviews score` = n_minimum_nights) ~ N + min + 
                             max + mean, data = df, title = "Minimum nights Summary")
sum_nnights

ggplot(df, aes(n_minimum_nights)) +
  geom_histogram( fill = "#440154", color = "white", alpha = 0.6, size = 0.25, binwidth = 1) +
  xlim(0,50)+
  xlab("N of minimum nights") +
  theme_bw()

# Pool and categorize the number of minimum nights: 1,2,3, 3+
df <- df %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,31,max(df$n_minimum_nights)), labels=c(1,2,3), right = F))

#----------------------------
# Time since
# Create variables, measuring the time since: squared, cubic, logs
df <- df %>%
  mutate(
    ln_days_since = log(n_days_since),
    ln_days_since2 = log(n_days_since)^2,
    ln_days_since3 = log(n_days_since)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3)

# Check the effect
lndays_plot <- df %>%
  filter(df$price<=800, ln_days_since>2)

skimr::skim(df$n_number_of_reviews)
ggplot(data = df, aes(x=n_number_of_reviews , y=price)) +
  geom_point(size=1.5, colour= "#440154") +
  ylim(60,100)+
  xlim(0,20)+
  geom_smooth(method="loess", colour="#440154", se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_bw()

#----------------------------
# pool categories
# get host_response_time as factors



# Squares and further values to create
df <- df %>%
  mutate(n_accommodates2=n_accommodates^2, 
         ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2,
         ln_beds = log(n_beds),
         ln_number_of_reviews = log(n_number_of_reviews+1)
  )

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)


#----------------------------
# Dealing with missing values                    

# where do we have missing variables?
to_filter <- sort(sapply(df, function(x) sum(is.na(x)/nrow(df)*100)))
to_filter[to_filter > 0]

# what to do with missing values? 
# 1. drop if no target
df <- df %>%
  drop_na(price)


# 2. impute when few, not that important
df <- df %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds),
  ) 

# 3. drop columns when many missing not important
df <- df %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), 1, n_bathrooms), #assume 1 bath where it is missing
    n_bedrooms=ifelse(is.na(n_bedrooms), ifelse(n_accommodates<=4, 1, 2), n_bedrooms), 
    n_beds = ifelse(is.na(n_beds), round2(n_accommodates/2, 0), n_beds), #assume n_beds=n_accomodates/2 (mostly double beds)
    d_host_is_superhost = ifelse(is.na(d_host_is_superhost), 0, d_host_is_superhost),
    d_host_identity_verified = ifelse(is.na(d_host_identity_verified), 0, d_host_identity_verified)
  )
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# 4. Replace missing variables re reviews with zero, when no review + add flags
df <- df %>%
  mutate(
    flag_days_since=ifelse(is.na(n_days_since),1, 0),
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month),
    flag_n_number_of_reviews=ifelse(n_number_of_reviews==0,1, 0)
  )
table(df$flag_days_since)

# redo features
# Create variables, measuring the time since: squared, cubic, logs
df <- df %>%
  mutate(
    ln_days_since = log(n_days_since+1),
    ln_days_since2 = log(n_days_since+1)^2,
    ln_days_since3 = log(n_days_since+1)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3,
    ln_review_scores_rating = log(n_review_scores_rating),
    ln_days_since=ifelse(is.na(ln_days_since),0, ln_days_since),
    ln_days_since2=ifelse(is.na(ln_days_since2),0, ln_days_since2),
    ln_days_since3=ifelse(is.na(ln_days_since3),0, ln_days_since3),
  )


df <- df %>%
  mutate(p_host_response_rate = ifelse(is.na(p_host_response_rate),1, p_host_response_rate ))
df$p_host_response_rate <- df$p_host_response_rate %>% replace(df$p_host_response_rate == "N/A", 1)


# where do we have missing variables now?
to_filter <- sort(sapply(df, function(x) sum(is.na(x)/nrow(df)*100)))
to_filter[to_filter > 0]

# save workfile
write.csv(df, paste0(data_out, "airbnb_ny_cleaned.csv"))

