install.packages('tidygeocoder')
library("tidygeocoder")
library(tidygeocoder)
library(tidyverse)



browseVignettes(package = "zctaCrosswalk")

sponsor_geo_info <- left_join(
    days_till_reunification,
    zip_info,
    by = c("Sponsor.Zipcode" = "ZIP_CODE")
) %>%
    rename_with(str_to_lower)





### TEXAS

texas_releases <- sponsor_geo_info %>% geocode(postalcode = "zcta")

