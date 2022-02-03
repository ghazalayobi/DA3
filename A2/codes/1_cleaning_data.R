
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


getwd()

setwd("/Users/ghazalayobi/DA3/A2")

path <- "//Users/ghazalayobi/DA3/A2"

#location folders
data_in  <- paste0(path,"/data/raw/")
data_out <- paste0(path,"/data/clean/")


# INITIAL STEPS ----------------------------------------------------------------
df <- read.csv(paste0(data_in,"listings.csv"), fileEncoding="UTF-8")

sum(rowSums(is.na(df)) == ncol(df)) # to check if there is only NA rows
nrow(df[duplicated(df),]) # to check if there  are duplicates


# Droping unnecessary columns --------------------------------------------------
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
           "host_response_time", 
           "summary", 
           "space", 
           "host_location",
           "host_total_listings_count",
           "bathrooms", # Contains NA values
           "neighbourhood_group_cleansed",
           "minimum_minimum_nights","maximum_maximum_nights","minimum_maximum_nights",
           "maximum_minimum_nights","minimum_nights_avg_ntm","maximum_nights_avg_ntm", 
           "number_of_reviews_ltm", "is_business_travel_ready", 
           "calculated_host_listings_count_entire_homes", 
           "calculated_host_listings_count_private_rooms", 
           "calculated_host_listings_count_shared_rooms")


df <- df[ , !(names(df) %in% drops)]



write.csv(df,file=paste0(data_in,"airbnb_ny_listing.csv"))
rm(df,drops)
#--------------------------------------------------------------------------------

# Import data
df <- read.csv(paste0(data_in,"airbnb_ny_listing.csv"),fileEncoding="UTF-8")


#drop broken lines - where id is not a character of numbers
df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df$junk <- NULL

#display the class and type of each columns
sapply(df, class)
sapply(df, typeof)


#-----------------------------------------------------------------------------

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

#Amenities--------------------------------------------------------------------------

df$amenities<-gsub("\\[","",df$amenities)
df$amenities<-gsub("\\]","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-gsub('2013',"", as.character(df$amenities)) #removing \u2013
df$amenities<-gsub('2014',"", as.character(df$amenities)) #removing \u2014
df$amenities<-gsub('2019s',"", as.character(df$amenities)) #removing \u2019s
df$amenities<-gsub('2019',"", as.character(df$amenities)) # removing \u2019
df$amenities<-gsub('\\\\u',"", as.character(df$amenities)) # removing \u
df$amenities<-gsub('Kitchen Aid refrigerator',"refrigerator", as.character(df$amenities))
df$amenities<-gsub(' KitchenAid stainless steel gas stove',"stove", as.character(df$amenities))
df$amenities<-gsub(' Kitchenaid stainless steel oven',"oven", as.character(df$amenities))
df$amenities<-gsub(' kitchenaid stainless steel oven',"oven", as.character(df$amenities))


df$amenities <- df$amenities<-as.list(strsplit(df$amenities, ","))


#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))
df<-cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
df <- df[ , !(names(df) %in% drops)]


# create data frame of the amenities

amts <- df %>% select(-(1:50))


# delete spaces in the beginning and end of the column names, and transfer all to lower case
names(amts) <- gsub(" ","_", tolower(trimws(names(amts))))
names(df) <- tolower(names(df))



# list of key words Key words

column_names <- c( "kitchen", "stove|gas_stove", "oven|steel_oven|stainless_steel_oven", 
                   "frige|refrigerator|mini_fridge",
                   "coffee|nespresso_machine|keurig_coffee_machine", 
                   "microwave",
          "wifi|internet|ethernet_connection", "tv|hdtv|cable_tv|v_with_standard_cable", "sound_ystem|speaker",
          "toiletries", "shampoo|conditioner", "hair_dryer", "washer", "dryer", "iron", "hot_water", 
          "heating|heated_floor|fireplace|central_heating", "air_conditioning|fan|ac|central_air_conditioning", 
          "breakfast", "fitness|.*gym.*",  
          "children|baby|crib", "smoking", 
          "long_term_stays_allowed",
          "free.*on premises", "free.*street", "paid.*off premises|self-parking",
          "clothing_storage|clothing_storage.*",
          "smoke_alarm|carbon_monoxide_alarm"
          
)





# function to merge columns with the same key word in them
for (i in column_names) {
  tdf <- amts %>% select(matches(i))
  
  amts$new_col <- ifelse(rowSums(tdf)>0, 1, 0)
  
  names(amts)[names(amts) == "new_col"] <- paste0("have_", i)
  
  amts <- amts %>% select(-colnames(tdf)) 
  
} 

# keep only columns where the percentage of 1s is at least 1% and at most 99%
selected <- sapply(names(amts), function(x){
  ratio <- sum(amts[[x]])/nrow(amts)*100
  if (between(ratio, 1, 99)) {
    return(TRUE)
  } else { return(FALSE) }
})

amenities <- amts[,selected]
names(amenities)


amenities <- amenities %>% select((1:110))


df <- df %>% select((1:49))

df <- cbind(df, amenities)

#write csv
write.csv(df,file=paste0(data_out,"airbnb_ny_workfile.csv"))








