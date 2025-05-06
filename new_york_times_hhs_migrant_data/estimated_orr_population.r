source(
    "/workspaces/NYT_HHS_MIGRANT/new_york_times_hhs_migrant_data/read_in_data.r",
    encoding = "UTF-8"
)

month_seq <- seq(
    min(hhs_migrant_data$date_of_entry),
    max(hhs_migrant_data$date_of_release),
    by = "month"
)

day_seq <- seq(
    min(hhs_migrant_data$date_of_entry),
    max(hhs_migrant_data$date_of_release),
    by = "day"
)

week_seq <- seq(
    min(hhs_migrant_data$date_of_entry),
    max(hhs_migrant_data$date_of_release),
    by = "week"
)



estimate_population <- function(date_i) {
    days_till_reunification %>%
        summarise(
            estimate_orr_pop = length(
                ID[
                    date_of_release > as.Date(date_i) &
                    date_of_entry <= as.Date(date_i)
                ]
            ),
            arrivals = length(ID[date_of_entry <= as.Date(date_i) & date_of_entry > (as.Date(date_i) - 7)] ),
            releases = length(ID[date_of_release <= as.Date(date_i) & date_of_release > (as.Date(date_i) - 7)]) 
        )
}






estimate_pop_list = list()

for (i in week_seq) {
    df <- estimate_population(i)

    df$pop_date <- as.Date(i)

    estimate_pop_list[[i]] <- df
}

estimate_orr_pop = do.call(rbind, estimate_pop_list)


estimate_orr_pop %>%
    pivot_longer(-c(pop_date)) %>%
    #filter(!name %in% c("cum_arrivals", "cum_releases")) %>%
    ggplot(aes(x = pop_date, y = value)) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(
        labels = c("Arrivals", "Estimated ORR pop", "Releases"),
        values = c("red", "black", "green")
    ) +
    geom_line(aes(color = name), linewidth = 1.5) +
    xlab('Weeks') +
    ylab('') +
    ggtitle("# of Kids", subtitle = "Source: NYT") +
    theme_classic() +
    theme(text = element_text(size = 30)) +
    annotate(
        'rect',
        xmin = as.Date("2015-01-01"),
        xmax = as.Date("2016-01-21"),
        ymin = 0,
        ymax = 22000,
        fill = "green",
        alpha = 0.3
    ) +
    annotate(
        'text',
        x = as.Date("2015-06-25"),
        y = 15000,
        label = "Obama Admin",
        size = 6
    ) +
    annotate(
        'rect',
        xmin = as.Date("2016-01-22"),
        xmax = as.Date("2021-01-21"),
        ymin = 0,
        ymax = 22000,
        fill = "red",
        alpha = 0.3
    ) +
    annotate(
        'text',
        x = as.Date("2018-06-01"),
        y = 15000,
        label = "Trump Admin",
        size = 10
    ) +
    annotate(
        'rect',
        xmin = as.Date("2021-01-22"),
        xmax = as.Date("2023-06-01"),
        ymin = 0,
        ymax = 22000,
        fill = "blue",
        alpha = 0.3
    ) +
    annotate(
        'text',
        x = as.Date("2022-06-01"),
        y = 15000,
        label = "Biden Admin",
        size = 10
    )


