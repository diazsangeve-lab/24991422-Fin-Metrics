# Reshapes one or more fund statistics tables so that the measures run down the side and each fund and period is a column, with readable formatting.
Format_Stats_Table <- function(df){

    if( !all(c("Name", "Period") %in% names(df)) ) stop("... for Format_Stats_Table to work, provide columns called 'Name' and 'Period'") # stop early if the labels are missing

    Pct_Rows <- c("Return (Ann.)", "SD (Ann.)", "Max Drawdown", "Modified CVaR (95%)", "Tracking Error") # measures that read best as percentages

    df %>%
        mutate(Column = glue::glue("{Name} {Period}")) %>% # one column label per fund and period
        select(-Name, -Period) %>% # the labels are now in the column name
        gather(Measure, Value, -Column) %>% # long format, one row per measure and column
        mutate(Value = ifelse(is.na(Value), "-", ifelse(Measure %in% Pct_Rows, scales::percent(Value, accuracy = 0.1), sprintf("%.2f", Value)))) %>% # percentages where they make sense, two decimals elsewhere and a dash for blanks
        mutate(Measure = factor(Measure, levels = unique(Measure))) %>% # keep the measures in their original order
        spread(Column, Value) # one column per fund and period

}
