# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Script that scrapes race results data from www.idahomtb.org.

# Resources
library(tidyverse)
library(rvest)
library(lubridate)
library(feather)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Get table of race result URLs ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Results home
results_home <- "https://www.idahomtb.org/program-overview/race-series/results-2/"

# Get html for everything in the accordion
accordion_items <- read_html(results_home) %>%
    html_elements(".elementor-accordion-item")

# Get html for all tabs under the accordion. Each tab is a season.
tab_content <- accordion_items %>%
    html_element(".elementor-tab-content")

# Loop over tabs
races <- map_dfr(tab_content, function(tab) {
    
    # Extract race names & dates (marked with <strong> tags) and results URLs to
    # character vector.
    race_chr <- as.character(tab) %>%
        str_extract_all("<strong>.+</strong>|http.+?html?") %>%
        .[[1]] %>%
        str_remove_all("<strong>|</strong>")
    
    # Indices for races. Start indices are the race name/data; end indices are
    # any results URLs associated witht he race.
    race_start <- which(!str_detect(race_chr, "htm"))
    race_end <- race_start - 1
    race_end <- c(race_end[2:length(race_end)], length(race_chr))
    
    # Build list with race names and associated URLs grouped together
    race_list <- map2(race_start, race_end, function(x, y) {
        race_chr[x:y]
    })
    
    # Convert race_list into df with cols for race, url, date
    race_df <- map_dfr(race_list, function(x) {
        list_length <- length(x)
        tibble(loc = x[1],
               url = x[2:list_length])
    }) %>%
        
        # Drop any reporting on series, team, or overall rider standings
        filter(!str_detect(loc, "Series"),
               !str_detect(url, "[Tt]eam"),
               !str_detect(url, "[Oo]verall")) %>%
        
        # Remove stray html tags
        mutate(loc = str_remove(loc, "<.+>"),
               
               # Fix date typo
               loc = str_replace(loc, "9/25/121", "9/25/21"),
               
               # Pull dates as last 8 digits in loc strings
               date = mdy(str_trim(str_sub(loc, -8))),
               
               # Trim whitespace
               loc = str_trim(str_extract(loc, "[a-zA-Z/ ]+")),
               
               # Standardize loc names
               loc = case_when(
                   str_detect(loc, "[Bb]rundage") ~ "Brundage",
                   str_detect(loc, "[Mm]agic") ~ "Magic",
                   str_detect(loc, "[Jj]ug") ~ "Jug",
                   str_detect(loc, "[Gg]alena") ~ "Galena",
                   str_detect(loc, "[Tt]arghee|[Gg]rand") ~ "Targhee",
                   str_detect(loc, "[Aa]vimor") ~ "Avimor",
                   str_detect(loc, "[Ee]agle") ~ "Eagle",
                   str_detect(loc, "[Mm]cNabb") ~ "McNabb",
                   str_detect(loc, "[Bb]ogus") ~ "Bogus"),
               
               # Make the URL NA for locs in the future
               url = if_else(!str_detect(url, "http"), NA_character_, url),
               
               # Replace all http with https
               url = str_replace(url, "http:", "https:"))
}) %>%
    select(loc, date, url)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Get individual race results ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

desired_cols <- paste0("X", as.character(1:11))
desired_cols2 <- "Lap 5"

# Loop over individual result pages
indiv_all <- pmap_dfr(races, function(loc, date, url) {
    
    # User message
    message(paste("Processing", loc, date, url, "..."))
    
    # 2015 results are in a different format. Note that they're coming from
    # nationalmtb.org, not idahomtb.org.
    if(year(date) == "2015" & !is.na(url)) {
        
        # Get all tables
        tables_2015 <- read_html(url) %>%
            html_elements("table") %>%
            html_elements("table") %>%
            html_table(na.strings = c("NA", ""))
        
        # Extract individual results
        temp <- map_dfr(tables_2015[seq(2, length(tables_2015), 2)], function(x) {
            current_df <- x
            division <- current_df[1,]$X1
            start_idx <- which(current_df$X1 == 1)
            end_idx <- nrow(current_df)
            current_df[start_idx:end_idx,] %>%
                mutate(div = division)
            })
        
        # Add any missing cols as NA
        temp[setdiff(desired_cols, names(temp))] <- NA_character_
        
        # More cleanup
        temp %>%
            select(-X3,-X5, -X6) %>%
            rename(Place = X1, Name = X2, Team = X4, `Lap 1` = X7,
                   `Lap 2` = X8, `Lap 3` = X9, `Lap 4` = X10, `Lap 5` = X11) %>%
            mutate(
                   Plate = str_extract(Name, "\\([0-9]+\\)"),
                   Plate = str_remove_all(Plate, "\\(|\\)"),
                   Name = str_extract(Name, ".+ \\(|\\).+"),
                   Name = str_remove(Name, "\\) | \\("),
                   gender = if_else(str_detect(div, "Girl"), "Female", "Male"),
                   div = str_remove(div, "Boys|Girls"),
                   div = str_remove(div, "\\(.+\\)"),
                   div = str_trim(div),
                   loc = loc,
                   date = ymd(date),
                   url = url)  %>%
            select(Place, Name, Plate, Team, `Lap 1`:`Lap 5`, loc, date, div,
                   gender, url)
    }
    
    # All other years
    else if(year(date) > "2015" & !is.na(url)) {
        
        # Get individual results for a race
        indiv <- read_html(url) %>%
            html_elements(xpath = "//table") %>%
            html_table(na.strings = c("NA", "")) %>%
            .[[1]]
        
        # Start & end indices are different for 2016 due to an extra row
        if(year(date) == "2016") {
            division_start <- which(indiv$X1 == "Place") - 2
            division_end <- division_start - 1
            division_end <- c(division_end[2:length(division_end)], nrow(indiv))
            indiv_names <- as.character(indiv[3,])
        } else {
            division_start <- which(indiv$X1 == "Place") - 1
            division_end <- division_start - 1
            division_end <- c(division_end[2:length(division_end)], nrow(indiv))
            indiv_names <- as.character(indiv[2,])
        }
        
        # Set column names
        names(indiv) <- indiv_names
        
        # Clean up
        temp2 <- map2_dfr(division_start, division_end, function(x, y) {
            current <- indiv[x:y,]
            division <- current[1,]$Place
            current %>% mutate(div = division)
        })
        
        # Add missing cols
        temp2[setdiff(desired_cols2, names(temp2))] <- NA_character_
        
        temp2 %>%
            filter(!str_detect(Place, "Males?|Females?|Boys|Girls|Place"),
                   Place != "") %>%
            
            # Add columns from races table
            mutate(loc = loc,
                   date = date,
                   url = url,
                   gender = if_else(str_detect(div, "Female|Girl"),
                                    "Female", "Male"),
                   div = str_remove(
                       div, "Female | Girls|Male |Males | Boys"),
                   
                   # For 2016, people who only did a single lap only have a
                   # total time recorded. Move this to Lap 1.
                   `Lap 1` = if_else(is.na(`Lap 1`) & !is.na(Total),
                                     Total, `Lap 1`)) %>%
            select(Place, Name, Plate, Team, `Lap 1`:`Lap 4`, `Lap 5`, loc,
                   date, div, gender, url, -Total)
    }
})

# Save to feather
write_feather(indiv_all, "data_wide.feather")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Function to convert time strings to numbers representing minutes ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function that converts character representations of time to minutes
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
    
    # Convert x representations and DNF to NA
    else if(str_detect(time_string, "^X X X X$|x:xx:xx|DNF"))
        { minutes <- NA_real_ }
    
    # Error code for anything not handled by the above categories
    else { minutes <- 99999 }
    return(minutes)
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Convert to long format ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

indiv_long <- indiv_all %>%
    pivot_longer(cols = starts_with("Lap"), names_to = "lap",
                 values_to = "time_char") %>%
    mutate(lap = as.integer(str_extract(lap, "[0-9]+")),
           time_min = to_minutes(time_char),
           Plate = as.integer(Plate),
           div = case_when(
               div == "Frosh" ~ "Freshman",
               div == "Soph" ~ "Sophomore",
               TRUE ~ div)) %>%
    select(Place:Team, Division = div, Gender = gender, Date = date,
           Location = loc, Lap = lap, time_char, time_min)

# Write to feather
write_feather(indiv_long, "data_long.feather")
