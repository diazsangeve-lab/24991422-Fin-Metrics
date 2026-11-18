# Builds one tidy data frame of monthly returns for every asset in the universe, from sources quoted as index levels, daily returns or monthly returns, using a meta table that names each asset and its source.
Asset_Returns_Build <- function(Meta, MSCI, Commods, Bond_Rets, Idx_Daily, LCL){ # define Asset_Returns_Build, with inputs Meta, MSCI, Commods, Bond_Rets, Idx_Daily and LCL

    if( !all(c("Asset", "Source", "Source_Name") %in% names(Meta)) ) stop("... Meta must have columns called 'Asset', 'Source' and 'Source_Name'") # stop early if the meta table is not in the expected shape

    Month_End <- function(d) lubridate::ceiling_date(d, "month") - 1 # the calendar month end of a date

    From_Levels <- function(df) df %>% mutate(YM = format(date, "%Y%m")) %>% group_by(Name, YM) %>% summarise(date = Month_End(max(date)), TRI = dplyr::last(TRI), .groups = "drop") %>% arrange(date) %>% group_by(Name) %>% mutate(Returns = TRI / lag(TRI) - 1) %>% ungroup() %>% filter(!is.na(Returns)) %>% select(date, Name, Returns) # month end levels turned into monthly returns

    From_Daily <- function(df) df %>% mutate(YM = format(date, "%Y%m")) %>% group_by(Name) %>% filter(YM > YM[which.min(date)] | lubridate::day(min(date)) <= 5) %>% group_by(Name, YM) %>% summarise(date = Month_End(max(date)), Returns = prod(1 + Returns) - 1, .groups = "drop") %>% select(date, Name, Returns) # daily returns compounded into months, dropping a first month that starts late

    bind_rows( # stack the monthly returns of every source into one tidy table
        From_Levels(MSCI) %>% mutate(Source = "MSCI"), # global equity indexes in dollars
        From_Levels(Commods) %>% mutate(Source = "Commods"), # commodity indexes in dollars
        Bond_Rets %>% select(date, Name, Returns) %>% mutate(Source = "Bonds"), # the constructed global bond indexes
        From_Daily(Idx_Daily %>% filter(!is.na(Returns))) %>% mutate(Source = "Local"), # local equity and factor indexes in rand
        LCL %>% transmute(date = Month_End(date), Name, Returns, Source = "LCL")) %>% # local bond and property indexes in rand
        inner_join(Meta, by = c("Source", "Name" = "Source_Name")) %>% # keep the chosen assets and attach their readable names and classes
        select(date, Asset, Returns, everything(), -Name, -Source) %>% # one tidy data frame
        arrange(Asset, date) # order each asset in time

}
