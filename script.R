library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)


##############################
## Loading files
##############################

# Load files into tibbles (using readr functions)
# col_types(): Manually define data type of each column as necessary

advertisers <- read_csv("data/advertiser.csv", 
                        col_types = 
                          cols(
                            ID = col_character() #amend parser guess (double) to string
                          )
)

campaigns <- read_csv("data/campaigns.csv" , 
                      col_types = 
                        cols(
                          id = col_character(),
                          advertiser_id = col_character()
                        )
)

clicks <- read_tsv("data/clicks.tsv", 
                   col_types = 
                     cols(
                       campaign_id = col_character(),
                       time = col_time(format = "%H:%M:%S"),
                       #date format needs to be specified because the original doesn't conform to ISO standard
                       date = col_date(format = "%d/%m/%Y")

                       
                     )
)

impressions <- read_tsv("data/impressions.tsv", 
                        col_types = 
                          cols(
                            campaign_id = col_character(),
                            time = col_time(format = "%H:%M:%S"),
                            date = col_date(format = "%d/%m/%Y")
                          )
)

##############################
## Utility functions
##############################

get_hour <- function(x){
  
  #' Gets the hours from a timestamp
  #'
  #' Takes a string with the correct time format, parses it and returns the hour element 
  #'
  #' x the input to be formatted

  format(strptime(x, "%H:%M:%S"),'%H')
}

get_mins_secs <- function(x){
  
  #' Gets the minutes and seconds from a timestamp
  #'
  #' Takes a string with the correct time format, parses it and returns the mins:secs elements 
  #'
  #' x the input to be formatted

  format(strptime(x, "%H:%M:%S"),'%M:%S')
}

time_format <- function(hour, x) {
  
  #' Format hour and timestamp (24hours, "%H:%M:%S")
  #'
  #' Transform an integer into the correct hour (0 to 23),
  #' concatenates the new value with the rest of the timestamp (minutes and seconds)
  #' and returns the new timestamp with the correct hour format
  #'
  #' hour is the positive integer, x the row (used to get the seconds and minutes)
  #' 

  if(hour >= 24){ #if the hour is 24 or more, then it restarts from 0 (and date is +1, handled by convert_dates)
    hour <- (hour %% 24)
  }
  new_time <- paste(str_pad(hour, 
                            width=2, pad="0"), #str_pad() adds leading zeros as necessary (ex.: 1 -> 01)
                    get_mins_secs(x["time"]),  
                    sep=":") #paste concatenates the two strings, hour and mins_secs, placing ":" between them
  return(new_time)
  
}


convert_time <- function(x){
  
  #' Convert EST and PST to UTC
  #'
  #' Adds the respective time difference to the timestamp
  #'
  #' x is the row, to be used in conjunction with apply()
  #' 
  
  hour <- x["time"]
  if(x["timezone"] != "UTC"){ #enter if it's not UTC already
    hour <- as.numeric(get_hour(hour)) # transform to numeric value to apply arithmetic operations
    if(x["timezone"] == "Eastern time"){
      hour <- hour + 5
    } 
    if(x["timezone"] == "Pacific time"){
      hour <- hour + 8
    }
    hour <- time_format(hour, x)
  }
  return(hour) #returns transformed hour OR the same hour if already UTC
}

 
convert_dates <- function(x){
  
  #' Convert EST and PST dates to UTC
  #'
  #' Convert dates to next calendar day if the hour was transformed and went over 24h
  #'
  #' x is the row, to be used in conjunction with apply()
  #'

  is_different_date <- get_hour(x["new_time"]) <  get_hour(x["time"])
  return(ifelse(is_different_date,              #condition
                format(as.Date(x["date"]) + 1), #result if true
                x["date"])                      #result if false
         ) 
  
}

##############################
## Convert to UTC
##############################



convert_to_UTC <- function(my_tibble) {
  
  #' Convert time, date and timezone columns to UTC
  #'
  #' Applies time and date transformations rowwise
  #'
  #' my_tibble is the tibble to be transformed
  #'
  
  new_times <- apply(my_tibble, 1, convert_time) # the second argument, 1, indicates it's rowwise
  temp_tibble <- mutate(my_tibble, new_time=new_times) 
  new_dates <- apply(temp_tibble, 1, convert_dates)
  temp_tibble$new_dates = new_dates
  return(temp_tibble)
}

clicks2 <- convert_to_UTC(clicks)
impressions2 <- convert_to_UTC(impressions)

##############################
## Join advertisers and campaigns
##############################

extend_tbl <- function(orig_tibble, updated_tibble) {
  
  #' Creates a tibble with dates and times in UTC format, joins advertiser and campaign data 
  #'
  #' Puts converted times and date into date and time columns, 
  #' joins with campaings and advertisers info
  #'
  #' orig_tibble is the original data, updated contains dates and times converted to UTC
  #'
  
  
  mutate(orig_tibble, 
         time=updated_tibble$new_time, 
         date=updated_tibble$new_dates, 
         timezone="UTC") %>% 
  left_join(campaigns, by = c("campaign_id" = "id")) %>% 
  left_join(advertisers, by = c("advertiser_id" = "ID")) %>% 
  rename(campaign_name = name.x, advertiser_name = name.y)
}

new_clicks <- extend_tbl(clicks, clicks2)

new_impressions <- extend_tbl(impressions, impressions2)

##############################
## Save to csv
##############################

write_csv(new_impressions, "impressions_processed.csv")
write_csv(new_clicks, "clicks_processed.csv")

