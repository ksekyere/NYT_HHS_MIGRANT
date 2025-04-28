library(dplyr)

library(lubridate)
library(ggplot2)
library(stringr)





hhs_migrant_data <- read.csv("/Users/kforkuo/HHS MIGRANT DATA/hhs-child-migrant-data/data.csv") %>%
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
    mutate(days_till_reunification = date_of_release - date_of_entry)
