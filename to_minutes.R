# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Descrption: Function that converts character representations of time to
# minutes.

# Resources
library(stringr)
library(lubridate)

# Function definition
to_minutes <- Vectorize(function(time_string) {
    
    # Drop non-ascii characters
    time_string <- str_extract(time_string, "[[:ascii:]]+")
    
    # Keep NAs
    if(is.na(time_string)) { minutes <- NA_real_ }
    
    # Convert MM:SS or MM:SS.S format to minutes
    else if(str_detect(time_string, "^[0-9]{2}:[0-9]{2}(\\.[0-9])?$")) {
        time_period <- ms(time_string)
        minutes <- (minute(time_period) * 60 + second(time_period)) / 60 }
    
    # Convert H:MM:SS or H:MM:SS.S format to minutes
    else if(str_detect(time_string, "^[0-9]:[0-9]{2}:[0-9]{2}(\\.[0-9])?$")) {
        time_period <- hms(time_string)
        minutes <- (hour(time_period) * 60 * 60 + minute(time_period) * 60 +
                        second(time_period)) / 60 }
    
    # For McNabb 2021, times are to the hundredth of a second
    else if(str_detect(time_string, "^[0-9]:[0-9]{2}:[0-9]{2}\\.[0-9]{2}$")) {
        time_period <- hms(time_string)
        minutes <- (hour(time_period) * 60 * 60 + minute(time_period) * 60 +
                        second(time_period)) / 60 }
    
    # For H:MM:SS.SS, need to add "0:" to the start of the string in order for
    # hms() to work correctly.
    else if(str_detect(time_string, "^[0-9]{2}:[0-9]{2}\\.[0-9]{2}$")) {
        time_period <- hms(paste0("0:", time_string))
        minutes <- (hour(time_period) * 60 * 60 + minute(time_period) * 60 +
                        second(time_period)) / 60 
    }
    
    # Convert x representations and DNF to NA
    else if(str_detect(time_string, "^X X X X$|x:xx:xx|DNF"))
    { minutes <- NA_real_ }
    
    # Error code for anything not handled by the above categories
    else { minutes <- 99999 }
    return(minutes)
})
