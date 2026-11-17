# Calculates calendar year returns for every fund and the share of peers the client's fund beat in each year, treating the last incomplete year as year to date.
Calendar_Rank_Calc <- function(df, Fund_Label = "ABC Fund"){

    df %>%
        mutate(Year = lubridate::year(date)) %>% # the calendar year of each month
        group_by(Year) %>% # work year by year
        mutate(Months_Needed = n_distinct(lubridate::month(date))) %>% # the months that exist in the data for that year, twelve except in the final year
        group_by(Year, Name, Group, Months_Needed) %>% # one return per fund per year
        summarise(Months = n(), Ret = prod(1 + Returns) - 1, .groups = "drop") %>% # the compounded return in that year
        filter(Months == Months_Needed) %>% # only funds that were open for the whole year, so that returns are comparable
        group_by(Year) %>% # rank within each year
        filter(any(Group == Fund_Label)) %>% # only years in which the client's fund was open for the whole year
        summarise(Fund_Ret = Ret[Group == Fund_Label], # the fund's return in the year
                  Peer_Median = median(Ret[Group == "Peer"]), # the typical peer's return
                  Pct = mean(Ret[Group == "Peer"] < Fund_Ret), # the share of peers the fund beat
                  N_Peers = sum(Group == "Peer"), # the number of peers ranked
                  YTD = dplyr::first(Months_Needed) < 12, # whether the year is incomplete
                  .groups = "drop") %>%
        mutate(Quartile = cut(Pct, breaks = c(-Inf, 0.25, 0.5, 0.75, Inf), labels = c("Bottom quartile", "Third quartile", "Second quartile", "Top quartile"))) # the quartile the fund finished in

}
