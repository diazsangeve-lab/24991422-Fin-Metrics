# Calculates the rolling tracking error and information ratio of every fund against the benchmark over a trailing window of consecutive months, following the tracking error and information ratio measures of Practical 1.
Roll_TE_IR_Calc <- function(df, NSel = 36){

    df %>%
        filter(!is.na(BM)) %>% # only the months in which the benchmark is available
        mutate(Active = Returns - BM) %>% # each month's return above or below the benchmark
        arrange(date) %>% # order by date before rolling
        group_by(Name, Group) %>% # roll within each fund
        filter(n() >= NSel) %>% # funds need at least one full window
        mutate(TE = RcppRoll::roll_sd(Active, NSel, fill = NA, align = "right") * sqrt(12), # the annualised standard deviation of active returns
               IR = RcppRoll::roll_mean(Active, NSel, fill = NA, align = "right") * 12 / TE, # annualised active return per unit of tracking error
               Span = as.numeric(date - lag(date, NSel - 1))) %>% # the number of days each window covers
        ungroup() %>% # remove the grouping again
        filter(!is.na(TE), Span < NSel * 31) %>% # complete windows of consecutive months
        select(date, Name, Group, TE, IR) # keep only what is needed

}
