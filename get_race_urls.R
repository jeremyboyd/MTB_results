# Author: Jeremy Boyd (jeremyboyd@pm.me)
#D escription: Script that scrapes a table of representing all historical IICL
# races and writes it to feather.

# Resources
library(tidyverse)
library(rvest)
library(lubridate)

# Function definition
get_race_urls <- function() {
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
        
        # Extract race names & dates (marked with <strong> tags) and results
        # URLs to character vector.
        race_chr <- as.character(tab) %>%
            str_extract_all("<strong>.+</strong>|http.+?html?") %>%
            .[[1]] %>%
            str_remove_all("<strong>|</strong>")
        
        # Indices for races. Start indices are the race name/data; end indices
        # are any results URLs associated with the race.
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
            filter(
                !str_detect(loc, "Series")
                ,!str_detect(url, "[Tt]eam")
                ,!str_detect(url, "[Oo]verall")
            ) %>%
            
            # Remove stray html tags
            mutate(
                loc = str_remove(loc, "<.+>"),
                
                # Fix date typo
                loc = str_replace(loc, "9/25/121", "9/25/21"),
                
                # Pull dates as last 8 digits in loc strings
                date = mdy(str_trim(str_sub(loc,-8))),
                
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
                    str_detect(loc, "[Bb]ogus") ~ "Bogus",
                    TRUE ~ "Error"
                ),
                
                # Make the URL NA for locs in the future
                url = if_else(!str_detect(url, "http"), NA_character_, url),
                
                # Replace all http with https
                url = str_replace(url, "http:", "https:")
            )
    }) %>%
        select(loc, date, url) %>%
        arrange(desc(date)) %>%
        
        # Drop NA urls. These are races that are on the schedule but haven't
        # occurred yet.
        filter(!is.na(url))
    
    # Return table of historical races
    return(races)
}
