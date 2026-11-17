# Builds one tidy monthly data frame of state variables, from gauges quoted as levels and from series whose own volatility is the state.
State_Monthly_Calc <- function(Level_df, Level_Sel, Vol_df = NULL, Vol_Sel = NULL, Ann = 252){

    if( !all(c("date", "Name", "TRI") %in% names(Level_df)) ) stop("... Level_df must have columns called 'date', 'Name' and 'TRI'") # stop early if the gauge data is not in the expected shape

    Levels <-
        Level_df %>%
        filter(Name %in% Level_Sel) %>% # keep only the chosen gauges
        mutate(YM = format(date, "%Y%B")) %>% # add a year and month label
        group_by(State = Name, YM) %>% # one number per gauge per month
        summarise(Value = mean(TRI, na.rm = T), .groups = "drop") # the average level of the gauge in that month

    if( is.null(Vol_df) ) return(Levels) # if no volatility based states were asked for, the levels are the answer

    Vols <-
        Vol_df %>%
        filter(Name %in% Vol_Sel) %>% # keep only the chosen series
        mutate(YM = format(date, "%Y%B")) %>% # add a year and month label
        group_by(State = paste0(Name, "_Vol"), YM) %>% # one number per series per month, with a label that says it is a volatility
        summarise(Value = sd(Returns, na.rm = T) * sqrt(Ann), .groups = "drop") # the realised volatility of the series inside that month

    bind_rows(Levels, Vols) # stack the two kinds of state variable into one tidy data frame

}
