source(
    "/workspaces/NYT_HHS_MIGRANT/new_york_times_hhs_migrant_data/read_in_data.r",
    encoding = "UTF-8"
)



estimate_pop_list = list()

for(i in month_seq){
    
    df <- estimate_population(i)
    
    estimate_pop_list[[i]] <- df

}

estimate_orr_poo = do.call(rbind, estimate_pop_list)


estimated_orr_pop %>%
    pivot_longer(-c(arrival_month, release_month)) %>%
    filter(!name %in% c("cum_arrivals", "cum_releases")) %>%
    ggplot(aes(x = arrival_month, y = value)) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(
        labels = c("Arrivals", "Estimated ORR pop", "Releases"),
        values = c("red", "black", "green")
    ) +
    geom_line(aes(color = name), linewidth = 1.5) +
    xlab('') + ylab('') + ggtitle("# of Kids") +
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


