days_till_reunification %>%
    arrange(-days_till_reunification) %>%
    head(7) %>%
    View


days_till_reunification %>%
    mutate(,
        fiscal_year  = as.integer(
            as.yearmon(date_of_release) - 9 / 12 + 1
            ),
        cal_year_of_release = year(date_of_release)
    ) %>%
    group_by(release_month) %>%
    summarise(avg.time.to.reunify = mean(days_till_reunification)) 



estimated_pop_jan_2016 = days_till_reunification %>%
filter(date_of_release <= as.Date("2016-02-01"))




library(dplyr)

# Define the function

estimate_pop_list = list()

for(i in month_seq){
    
    df <- estimate_population(i)
    
    estimate_pop_list[[i]] <- df

}

estimate_orr_population = do.call(rbind, estimate_pop_list)

estimate_orr_population("2021-05-01")

# Example usage
month_seq = as.character(seq(
    min(hhs_migrant_data$date_of_entry),
    max(hhs_migrant_data$date_of_entry),
    by = "month"
))

estimate_population(days_till_reunification, dates)




days_till_reunification %>% 
summarise(
estimate_pop_jan_2015 = length(ID[date_of_release <= as.Date("2015-02-01")]), 
estimate_pop_feb_2015 = length(ID[date_of_release <= as.Date("2015-03-01")]), 
estimate_pop_jan_2016 = length(ID[date_of_release <= as.Date("2016-02-01")])
)



library(dplyr)
GraphVar = "dist"
cars %>%
     group_by(.data[["speed"]]) %>%
     summarise(Sum = sum(.data[[GraphVar]], na.rm = TRUE),
               Count = n() )
