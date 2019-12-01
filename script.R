
#setwd("C:/Users/Catarina/DIT/StatProg/ca2/statsprog-R-ca2")

library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)

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
                       date = col_date(format = "%d/%m/%Y"), #date format needs to be specified because the original doesn't conform to ISO standard
                       time = col_time()
                      )
)

impressions <- read_tsv("data/impressions.tsv", 
                        col_types = 
                          cols(
                            campaign_id = col_character(),
                            date = col_date(format = "%d/%m/%Y"),
                            time = col_time()
                          )
)

##############################################################################

get_hour <- function(x){
  format(strptime(x, "%H:%M:%S"),'%H')
}

get_mins_secs <- function(x){
  format(strptime(x, "%H:%M:%S"),'%M:%S')
}

new_clicks <- mutate(clicks, hours = format(strptime(clicks$time, "%H:%M:%S"),'%H'))
# 
# new_clicks <- mutate(new_clicks, hours2 = EST_to_UTC(new_clicks$hours))

# EST_to_UTC <- function(x){
#   result <- as.numeric(get_hour(x["time"])) + 5
#   if(result %% 24){
#     result <- (result %% 24)
#   }
#   return(str(result))
# }

# hours <- apply(new_clicks, 1, EST_to_UTC)
# new_clicks$hours2 = hours

time_format <- function(hour, x) {
  if(hour >= 24){ #if the hour is 24 or more, then it restarts from 0 (and date is +1, handled elsewhere)
    hour <- (hour %% 24)
  }
  new_time <- paste(str_pad(hour, width=2, pad="0"), get_mins_secs(x["time"]),  sep=":")
  return(new_time)
}

# x is the row
convert_timezone <- function(x){
  hour <- x["time"]
  if(x["timezone"] != "UTC"){
    hour <- as.numeric(get_hour(hour))
    if(x["timezone"] == "Eastern time"){
      hour <- hour + 5
    } 
    if(x["timezone"] == "Pacific time"){
      hour <- hour + 8
    }
    hour <- time_format(hour, x)
  }
  return(hour)
}

convert_dates <- function(x){
  is_different_date <- get_hour(x["new_time"]) <  get_hour(x["time"])
  return(ifelse(is_different_date, format(as.Date(x["date"]) + 1),  x["date"]))
}

###################

new_times <- apply(clicks, 1, convert_timezone)
clicks2 <- mutate(clicks, new_time=new_times) 
new_dates <- apply(clicks2, 1, convert_dates)
clicks2$new_dates = new_dates



new_times_i <- apply(impressions, 1, convert_timezone)
impressions2 <- mutate(impressions, new_time=new_times_i) 
new_dates_i <- apply(impressions2, 1, convert_dates)
impressions2$new_dates = new_dates_i

###################

new_clicks <- mutate(clicks, 
                     time=clicks2$new_time, 
                     date=clicks2$new_dates, 
                     timezone="UTC") %>% 
  left_join(campaigns, by = c("campaign_id" = "id")) %>% 
  left_join(advertisers, by = c("advertiser_id" = "ID")) %>% 
  rename(campaign_name = name.x, advertiser_name = name.y)



new_impressions <- mutate(impressions, 
                          time=impressions2$new_time, 
                          date=impressions2$new_dates, 
                          timezone="UTC") %>% 
  left_join(campaigns, by = c("campaign_id" = "id")) %>% 
  left_join(advertisers, by = c("advertiser_id" = "ID")) %>% 
  rename(campaign_name = name.x, advertiser_name = name.y)

###################

write_csv(new_impressions, "impressions_processed.csv")
write_csv(new_clicks, "clicks_processed.csv")

###########################################################################


