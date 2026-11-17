# Chains daily returns into calendar month returns for every series, labelling each month with its last trading day.
Monthly_Returns_Calc <- function(df){

    if( !all(c("date", "Name", "Returns") %in% names(df)) ) stop("... df must have columns called 'date', 'Name' and 'Returns'") # stop early if the data is not in the expected shape

    df %>%
        mutate(YM = format(date, "%Y%B")) %>% # a year and month label
        group_by(Name, YM) %>% # one number per series per month
        summarise(date = max(date), Returns = prod(1 + Returns) - 1, .groups = "drop") %>% # compound the daily returns and keep the last trading day as the date
        arrange(date) # order by date

}
