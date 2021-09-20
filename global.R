# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: global.R for a Shiny app that shows times from Idaho
# Interscholastic Cycling League races.

# Load resources
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(dplyr)
library(forcats)
library(lubridate)
library(feather)
source("get_race_urls.R")
source("get_race_data.R")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Define & set custom ggplot theme ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

my_theme <- function() {
    theme_classic() %+replace%
        theme(
            panel.grid.major = element_line(color = "gray90", size = .3)
            ,panel.spacing = unit(1, "lines")
            ,strip.background = element_rect(color = NA)
            ,strip.text = element_text(size = 10)
            ,axis.title.y = element_text(angle = 0, vjust = .5)
        )
}
theme_set(my_theme())

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Check for new data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# In production this will be run from the polished server. Will the scraping actually work from there? Need to test.
# Add a progress bar for scraping?
# Only scrape for the first user of the week?
# Faster if I store data in wide format?

# Read in old table of race URLs
race_urls_old <- read_feather("race_urls_old.feather")

# Get up-to-date table of race URLs
race_urls_new <- get_race_urls()

# Returns rows representing new URLs
new_urls <- anti_join(race_urls_new, race_urls_old)

# Do this if there are new URLs
if(nrow(new_urls) > 0) {
    
    # Scrape new data
    new_data <- get_race_data(new_urls)
    
    # Read in old data
    old_data <- read_feather("data_long.feather")
    
    # Combine old and new data
    updated_data <- bind_rows(new_data, old_data) %>%
        arrange(desc(Date), Division, Gender)
    
    # Write updated data to file
    write_feather(updated_data, "data_long.feather")
    
    # Write updated historical race list to file
    write_feather(race_urls_new, "race_urls_old.feather" )
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Load data & set app variables ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Read in data
df <- read_feather("data_long.feather") %>%
    filter(!is.na(time_min)) %>%
    mutate(Race = as_factor(paste(Location, year(Date)))
           
           # Levels of Race are ordered from most to least recent
           ,Race = fct_reorder(Race, Date, .desc = TRUE)
           ,Division = paste(Division, Gender),
           
           # This is to give a clean version of time_char to show in tooltips
           `Lap Time` = time_char)

# List of riders in alphabetical order
rider_list <- df %>%
    pull(Name) %>%
    unique() %>%
    sort()

# List of divisions
division_list <- df %>%
    pull(Division) %>%
    unique() %>%
    sort()

# List of places starting with 1
place_list <- df %>%
    pull(Place) %>%
    unique()

# List of races
race_list <- levels(df$Race)

# Maximum number of riders that can be displayed
max_riders <- 10
