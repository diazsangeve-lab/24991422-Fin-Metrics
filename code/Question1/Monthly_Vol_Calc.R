# Calculates the realised volatility of every series within each calendar month, annualised.
Monthly_Vol_Calc <- function(df, Ann = 252, Min_Obs = 10){ # define Monthly_Vol_Calc, with inputs df, Ann and Min_Obs

    if( !all(c("date", "Name", "Returns") %in% names(df)) ) stop("... for Monthly_Vol_Calc to work, provide columns called 'date', 'Name' and 'Returns'") # stop early if the data is not in the expected shape

    df %>% # start from the input data
        mutate(Month_Date = lubridate::floor_date(date, unit = "month")) %>% # label every day with the first day of its month
        group_by(Name, Month_Date) %>% # one number per series per calendar month
        summarise(N = n(), Vol = sd(Returns, na.rm = T) * sqrt(Ann), .groups = "drop") %>% # the number of trading days and the annualised volatility inside the month
        filter(N >= Min_Obs) # drop months with too few trading days to give a reliable estimate

}
