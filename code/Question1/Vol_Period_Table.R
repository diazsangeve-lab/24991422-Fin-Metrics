# Compares annualised volatility across named sub-periods, and expresses the latest period relative to the full sample.
Vol_Period_Table <- function(df, Periods, Ann = 252){

    # For stepping through uncomment:
    # Periods <- tibble(Period = c("2015-2019", "2020"), Start = ymd(c(20150101, 20200101)), End = ymd(c(20191231, 20201231)))

    if( !all(c("Period", "Start", "End") %in% names(Periods)) ) stop("... Periods must have columns called 'Period', 'Start' and 'End'") # stop early if the period definitions are incomplete

    Full <-
        df %>%
        group_by(Name) %>% # one number per series
        summarise(SD = sd(Returns, na.rm = T) * sqrt(Ann)) %>% # annualised volatility over the whole sample
        mutate(Period = "Full Sample") # label it so that it can be bound to the sub-periods

    Sub <-
        Periods %>%
        group_split(Period) %>% # treat each sub-period on its own
        map_df(~df %>%
                   filter(date >= .x$Start, date <= .x$End) %>% # keep only the days that fall inside this sub-period
                   group_by(Name) %>% # one number per series
                   summarise(SD = sd(Returns, na.rm = T) * sqrt(Ann)) %>% # annualised volatility inside the sub-period
                   mutate(Period = .x$Period)) # label the rows with the sub-period name

    bind_rows(Sub, Full) %>%
        left_join(Full %>% select(Name, Full_SD = SD), by = "Name") %>% # attach the full sample number to every row
        mutate(Ratio = SD / Full_SD) %>% # a ratio above one means the period was more volatile than normal
        mutate(Period = factor(Period, levels = c(Periods$Period, "Full Sample"))) %>% # keep the periods in the order they were given
        arrange(Name, Period) # sort for easy reading

}
