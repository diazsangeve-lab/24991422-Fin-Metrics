# Aggregates daily log returns into weekly log returns, keeping only weeks that every series traded in.
Weekly_Returns_Calc <- function(df){

    if( !all(c("date", "Name", "dlogret") %in% names(df)) ) stop("... for Weekly_Returns_Calc to work, provide columns called 'date', 'Name' and 'dlogret'") # stop early if the data is not in the expected shape

    N_Series <- length(unique(df$Name)) # the number of series, used to check that a week is complete

    df %>%
        mutate(Week = lubridate::floor_date(date, unit = "week", week_start = 1)) %>% # label every day with the Monday of its week
        group_by(Name, Week) %>% # one number per series per week
        summarise(dlogret = sum(dlogret, na.rm = T), .groups = "drop") %>% # log returns add up over time, so the weekly return is the sum of the daily ones
        group_by(Week) %>% # now look across series within each week
        filter(n() == N_Series) %>% # keep only weeks in which every series has a return, so that the comparison is like for like
        ungroup() %>% # remove the grouping again
        select(date = Week, Name, dlogret) # rename the week label to date, as the downstream functions expect

}
