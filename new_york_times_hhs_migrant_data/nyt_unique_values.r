library(readxl)
library(openxlsx)
library(scales)




nyt_countries = hhs_migrant_data %>%
group_by(country_of_origin) %>%
summarise(kids = comma(n()))


nyt_gender <- hhs_migrant_data %>%
group_by(Child.s.Gender) %>%
summarise(kids = comma(n()))



nyt_zip_code <- hhs_migrant_data %>%
group_by(Sponsor.Zipcode) %>%
summarise(kids = comma(n()))  


nyt_sponsor_category <- hhs_migrant_data %>%
group_by(Sponsor.Category) %>%
summarise(kids = comma(n()))



nyt_sponsor_relationship <- hhs_migrant_data %>%
group_by(Relationship.of.Sponsor) %>%
summarise(kids = comma(n())) 



data_frames <- list("Countries" = nyt_countries, "Gender" = nyt_gender, 
"Zip Code" = nyt_zip_code, "Sponsor Category" = nyt_sponsor_category, 
"Sponsor Relationship" = nyt_sponsor_relationship)



write.xlsx(data_frames, "nyt_unique_values.xlsx")

