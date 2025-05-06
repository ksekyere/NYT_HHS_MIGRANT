source(
    "/workspaces/NYT_HHS_MIGRANT/new_york_times_hhs_migrant_data/read_in_data.r",
    encoding = "UTF-8"
)


#density plot -----------
days_till_reunification %>%
    filter(days_till_reunification < 200) %>%
    ggplot(aes(x = as.numeric(days_till_reunification))) +
    geom_density()


#line graphs -------------
days_till_reunification %>%
    mutate(Sponsor.Category = as.character(Sponsor.Category)) %>%
    group_by(release_month) %>%
    summarise(
        average_time_to_reunify = mean(days_till_reunification),
        fiscal_year_release = fiscal_year_release
    ) %>%
    ungroup() %>%
    ggplot(aes(x = release_month, y = average_time_to_reunify)) +
    geom_line(linewidth = 3) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    ggtitle(
        "Average Time To Reunify (calendar days)",
        subtitle = "Data Source NYT HHS Migrant data"
    ) +
    ylab("") +
    xlab("ORR Release Date")


days_till_reunification %>%
    mutate(Sponsor.Category = as.character(Sponsor.Category)) %>%
    group_by(arrival_month) %>%
    summarise(average_time_to_reunify = mean(days_till_reunification)) %>%
    ungroup() %>%
    ggplot(aes(x = arrival_month, y = average_time_to_reunify)) +
    geom_line(linewidth = 3) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    ggtitle(
        "Average Time To Reunify (calendar days)",
        subtitle = "Data Source NYT HHS Migrant data"
    ) +
    ylab("") +
    xlab("US Arrival Date")


days_till_reunification %>%
    mutate(
        release_month = rollback(date_of_release, roll_to_first = TRUE),
        arrival_month = rollback(date_of_entry, roll_to_first = TRUE)
    ) %>%
    mutate(Sponsor.Category = as.character(Sponsor.Category)) %>%
    mutate(
        country_of_origin = case_when(
            !country_of_origin %in%
                c("Guatemala", "Honduras", "El Salvador", "Mexico", "Ecuador") ~
                "Other",
            .default = as.character(country_of_origin)
        )
    ) %>%
    group_by(release_month, country_of_origin) %>%
    summarise(average_time_to_reunify = mean(days_till_reunification)) %>%
    ungroup() %>%
    # filter(country_of_origin %in% top_countries$country_of_origin) %>%
    ggplot(aes(x = release_month, y = average_time_to_reunify)) +
    geom_line(aes(color = country_of_origin), linewidth = 1) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    ggtitle(
        "Average Time To Reunify (calendar days)",
        subtitle = "Data Source NYT HHS Migrant data"
    ) +
    ylab("") +
    xlab("ORR Release Date")


days_till_reunification %>%
#filter(country_of_origin %in% top_countries$country_of_origin) %>%
    mutate(
        days_till_cat = case_when(
            days_till_reunification <= 5 ~ "<= 5 days",
            days_till_reunification <= 10 ~ "<= 10 days",
            days_till_reunification <= 15 ~ "<= 15 days",
            days_till_reunification <= 20 ~ "<= 20 days",
            days_till_reunification <= 25 ~ "<= 25 days",
            days_till_reunification <= 30 ~ "<= 30 days",
            days_till_reunification <= 35 ~ "<= 35 days",
            days_till_reunification <= 40 ~ "<= 40 days",
            days_till_reunification <= 50 ~ "<= 50 days",
            days_till_reunification <= 60 ~ "<= 60 days",
            days_till_reunification <= 70 ~ "<= 70 days",
            days_till_reunification <= 100 ~ "<= 100 days",
            days_till_reunification <= 200 ~ "<= 200 days",
            days_till_reunification > 200 ~ "> 200 days"
        ),
        days_till_cat = factor(
            days_till_cat,
            levels = c(
                "<= 5 days",
                "<= 10 days",
                "<= 15 days",
                "<= 20 days",
                "<= 25 days",
                "<= 30 days",
                "<= 35 days",
                "<= 40 days",
                "<= 50 days",
                "<= 60 days",
                "<= 70 days",
                "<= 100 days",
                "<= 200 days",
                "> 200 days"
            )
        )
    ) %>%
    mutate(country_of_origin = ifelse(country_of_origin %in% top_countries$country_of_origin, as.character(country_of_origin), "other")) %>%
    group_by(days_till_reunification, country_of_origin) %>%
    summarise(num_of_kids = n()) %>%
    ungroup() %>%
    ggplot(aes(x = days_till_reunification, y = num_of_kids)) +
    geom_bar(stat = "identity")  +
    scale_x_continuous(limits = c(0, 500)) + 
    scale_y_continuous(label = comma) +
    facet_grid(cols = vars(country_of_origin)) +
    #geom_vline(aes(xintercept = mean(num_of_kids, na.rm = TRUE)), color = "red") +
    ggtitle("# of Kids", subtitle = "Source: NYT (Jan 2015 - May 2023)") +
    ylab('') +
    xlab('days to reunify') +
    theme_classic() +
    theme(text = element_text(size = 20))


days_till_reunification %>%
 mutate(country_of_origin = ifelse(country_of_origin %in% top_countries$country_of_origin, as.character(country_of_origin), "other")) %>%
group_by(country_of_origin) %>%
summarise(avg = mean(days_till_reunification, na.rm = T))


