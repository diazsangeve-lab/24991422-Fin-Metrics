# Calculates the realised volatility of every series within each calendar month, annualised.
Monthly_Vol_Calc <- function(df, Ann = 252, Min_Obs = 10){

    if( !all(c("date", "Name", "Returns") %in% names(df)) ) stop("... for Monthly_Vol_Calc to work, provide columns called 'date', 'Name' and 'Returns'") # stop early if the data is not in the expected shape

    df %>%
        mutate(Year = lubridate::year(date), Month = lubridate::month(date, label = TRUE)) %>% # split the date into a year and a month label
        group_by(Name, Year, Month) %>% # one number per series per calendar month
        summarise(N = n(), Vol = sd(Returns, na.rm = T) * sqrt(Ann), .groups = "drop") %>% # the number of trading days and the annualised volatility inside the month
        filter(N >= Min_Obs) # drop months with too few trading days to give a reliable estimate

}
