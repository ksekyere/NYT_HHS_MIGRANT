library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(readxl)
library(tidyr)
library(tidyverse)
library(scales)



###HHS MIGRANT DATA  ---------------------------

hhs_migrant_data <- read.csv(
    "/workspaces/NYT_HHS_MIGRANT/new_york_times_hhs_migrant_data/data 2.csv"
) %>%
    rename(
        date_of_entry = "Child.s.Date.of.Entry",
        date_of_release = "Child.s.Date.of.Release",
        country_of_origin = "Child.s.Country.of.Origin"
    ) %>%
    mutate(
        date_of_entry = as.Date(date_of_entry, , "%m/%d/%y"),
        date_of_release = as.Date(date_of_release, "%m/%d/%y"),
        estimate_orr_entrance = date_of_entry + days(3)
    )


days_till_reunification <- hhs_migrant_data %>%
    mutate(days_till_reunification = date_of_release - date_of_entry) %>%
    mutate(
        release_month = rollback(date_of_release, roll_to_first = TRUE),
        arrival_month = rollback(date_of_entry, roll_to_first = TRUE)
    )


top_countries <- days_till_reunification %>%
    group_by(country_of_origin) %>%
    summarise(kids = n()) %>%
    ungroup() %>%
    arrange(desc(kids)) %>%
    slice(1:5) %>%
    select(country_of_origin) %>%
    mutate(country_of_origin = str_trim(country_of_origin))


### ZCTA info from Health Resources and Services Administration ----------

zip_info <- read_excel(
    '/workspaces/NYT_HHS_MIGRANT/new_york_times_hhs_migrant_data/ZIP Code to ZCTA Crosswalk.xlsx'
)


zcta_pop <- read.csv(
    '/workspaces/NYT_HHS_MIGRANT/new_york_times_hhs_migrant_data/zcta_population.csv'
) %>%
    slice(-1) %>%
    separate(NAME, c("type", "zcta")) %>%
    rename(zcta_pop = P1_001N) %>%
    mutate(pop_cat = ifelse(zcta_pop < 5000, 'rural', 'urban')) %>%
    select(zcta, zcta_pop, pop_cat)

geo_info <- left_join(zip_info, zcta_pop, by = 'zcta')
