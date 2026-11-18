# Turns a tidy total return index (TRI) data frame into daily simple and log returns per series.
TRI_Returns_Calc <- function(df, Start_Date = lubridate::ymd(20150101)){ # define TRI_Returns_Calc, with inputs df and Start_Date

    if( !"date" %in% names(df)) stop("... for TRI_Returns_Calc to work, provide a date column called 'date'") # stop early if there is no date column
    if( !"Name" %in% names(df)) stop("... for TRI_Returns_Calc to work, provide an id column called 'Name'") # stop early if there is no series name column
    if( !"TRI" %in% names(df)) stop("... for TRI_Returns_Calc to work, provide a price column called 'TRI'") # stop early if there is no index level column

    df %>% # start from the input data
        arrange(date) %>% # order by date, as lags are used below
        group_by(Name) %>% # calculate returns separately for each series
        mutate(Returns = TRI / lag(TRI) - 1) %>% # simple return, used for anything that is added across assets
        mutate(dlogret = log(TRI) - log(lag(TRI))) %>% # log return, used for the volatility models
        filter(date > dplyr::first(date)) %>% # drop the first row of each series, which has no return
        ungroup() %>% # remove the grouping again
        filter(date >= Start_Date) %>% # keep the sample from the chosen start date onwards
        select(date, Name, Returns, dlogret) # keep only the columns used downstream

}
