# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Given a tibble with race results, scrape and return all results.

# Resources
library(tidyverse)
source("to_minutes.R")

# Columns we need
desired_cols <- paste0("X", as.character(1:11))
desired_cols2 <- "Lap 5"

get_race_data <- function(races) {
    
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
    # write_feather(indiv_all, "data_wide.feather")
    
    
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
    
}
