library(shinydashboard)
library(shiny)









days_till_reunification %>%
    filter(days_till_reunification < 200) %>%
    ggplot(aes(x = as.numeric(days_till_reunification))) +
    geom_density()


days_till_reunification %>%
    mutate(
        release_month = rollback(date_of_release, roll_to_first = TRUE),
        arrival_month = month(date_of_entry)
    ) %>%
    mutate(Sponsor.Category = as.character(Sponsor.Category)) %>%
    group_by(release_month) %>%
    summarise(average_time_to_reunify = mean(days_till_reunification)) %>%
    ungroup() %>%
    ggplot(aes(x = release_month, y = average_time_to_reunify)) +
    geom_line(linewidth = 3) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    ggtitle("Average Time To Reunify (calendar days)", subtitle = "Data Source NYT HHS Migrant data") +
    ylab("") +
    xlab("ORR Release Date")



days_till_reunification %>%
    mutate(
        release_month = rollback(date_of_release, roll_to_first = TRUE),
        arrival_month = rollback(date_of_entry, roll_to_first = TRUE)
    ) %>%
    mutate(Sponsor.Category = as.character(Sponsor.Category)) %>%
    group_by(arrival_month, Sponsor.Category) %>%
    summarise(average_time_to_reunify = mean(days_till_reunification)) %>%
    ungroup() %>%
    ggplot(aes(x = arrival_month, y = average_time_to_reunify)) +
    geom_line(aes(color = Sponsor.Category), linewidth = 3) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    ggtitle("Average Time To Reunify (calendar days)", subtitle = "Data Source NYT HHS Migrant data") +
    ylab("") +
    xlab("US Arrival Date")



days_till_reunification %>%
    mutate(
        release_month = rollback(date_of_release, roll_to_first = TRUE),
        arrival_month = rollback(date_of_entry, roll_to_first = TRUE)
    ) %>%
    mutate(Sponsor.Category = as.character(Sponsor.Category)) %>%
    mutate(country_of_origin = case_when(
        !country_of_origin %in% c("Guatemala", "Honduras", "El Salvador", "Mexico", "Ecuador") ~ "Other",
        .default = as.character(country_of_origin)
    )) %>%
    group_by(release_month, country_of_origin) %>%
    summarise(average_time_to_reunify = mean(days_till_reunification)) %>%
    ungroup() %>%
    # filter(country_of_origin %in% top_countries$country_of_origin) %>%
    ggplot(aes(x = release_month, y = average_time_to_reunify)) +
    geom_line(aes(color = country_of_origin), linewidth = 1) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    ggtitle("Average Time To Reunify (calendar days)", subtitle = "Data Source NYT HHS Migrant data") +
    ylab("") +
    xlab("ORR Release Date")

names(days_till_reunification)

str(top_countries)



hhs_migrant_data %>%
    group_by(Sponsor.Category) %>%
    summarise(num_of_entries = length(unique(Relationship.of.Sponsor)))


hhs_migrant_data %>%
    filter(Sponsor.Category == 3) %>%
    select(Relationship.of.Sponsor) %>%
    pull(Relationship.of.Sponsor) %>%
    unique()


unique(Relationship.of.Sponsor)




names(days_till_reunification)



top_countries <- days_till_reunification %>%
    group_by(country_of_origin) %>%
    summarise(kids = n()) %>%
    ungroup() %>%
    arrange(desc(kids)) %>%
    slice(1:5) %>%
    select(country_of_origin)
mutate(country_of_origin = str_trim(country_of_origin))

mutate(country_of_origin = str_replace(country_of_origin, "\\\\", ""))




hhs_migrant_data %>%
    filter(is.na(Sponsor.Category)) %>%
    View()


hhs_migrant_data$country_of_origin %in% top_countries$country_of_origin
