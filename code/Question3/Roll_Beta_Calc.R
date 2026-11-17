# Calculates the rolling beta of every fund to the benchmark over a trailing window of months, keeping only windows of consecutive months.
Roll_Beta_Calc <- function(df, NSel = 36){

    if( !all(c("date", "Name", "Group", "Returns", "BM") %in% names(df)) ) stop("... for Roll_Beta_Calc to work, first run Fund_Universe_Prep") # stop early if the universe has not been prepared

    df %>%
        filter(!is.na(BM)) %>% # only the months in which the benchmark is available
        arrange(date) %>% # order by date before rolling
        group_by(Name, Group) %>% # roll within each fund
        filter(n() >= NSel) %>% # funds need at least one full window
        mutate(Sx = RcppRoll::roll_sum(BM, NSel, fill = NA, align = "right"), # rolling sum of benchmark returns
               Sy = RcppRoll::roll_sum(Returns, NSel, fill = NA, align = "right"), # rolling sum of fund returns
               Sxy = RcppRoll::roll_sum(BM * Returns, NSel, fill = NA, align = "right"), # rolling sum of cross products
               Sxx = RcppRoll::roll_sum(BM^2, NSel, fill = NA, align = "right"), # rolling sum of squared benchmark returns
               Beta = (NSel * Sxy - Sx * Sy) / (NSel * Sxx - Sx^2), # the least squares slope over each window
               Span = as.numeric(date - lag(date, NSel - 1))) %>% # the number of days each window covers
        ungroup() %>% # remove the grouping again
        filter(!is.na(Beta), Span < NSel * 31) %>% # keep complete windows that do not jump over a gap in the fund's history
        select(date, Name, Group, Beta) # keep only what is needed

}
