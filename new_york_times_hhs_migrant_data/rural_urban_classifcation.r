library(tidygeocoder)
library(tidyverse)
library(ggplot2)

#create a dataframe that has more detailed geo info
sponsor_geo_info <- left_join(
    days_till_reunification,
    geo_info,
    by = c("Sponsor.Zipcode" = "ZIP_CODE")
) %>%
    rename_with(str_to_lower)


sponsor_geo_info %>% filter(is.na(pop_cat)) %>% View

#breakdown of released by rural/urban classification
sponsor_geo_info %>%
    group_by(pop_cat, release_month) %>%
    #filter out the < 250 entries that do not have legit zip codes
    filter(!is.na(pop_cat)) %>%
    group_by(pop_cat, release_month) %>%
    #calculate total releases for each zcta
    summarise(total_kids = n()) %>%
    ungroup() %>%
    group_by(release_month) %>%
    #calculate % of releases to rural or urban areas for each month
    mutate(pct_of_total = total_kids / sum(total_kids)) %>%
    ungroup() %>%
    #create a line graph to show findings
    ggplot(aes(x = release_month, y = pct_of_total)) +
    #increase linewidth size
    geom_line(aes(color = pop_cat), linewidth = 2) +
    scale_y_continuous(labels = percent) +
    #update axis titles
    ylab('') +
    xlab('release month') +
    #update chart title
    ggtitle("Proportion of Rural Releases", subtitle = "Child Level") +
    #update legend title
    labs(color = 'Area Released') +
    # remove background 
    theme_classic() +
    theme(text = element_text(size = 30))
