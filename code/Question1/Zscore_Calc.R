# Standardises chosen columns into cross-sectional z-scores on each date, winsorised so that a few extreme stocks do not dominate.
Zscore_Calc <- function(df, Cols, Cap = 3){ # define Zscore_Calc, with inputs df, Cols and Cap

    if( !"date" %in% names(df)) stop("... for Zscore_Calc to work, provide a date column called 'date'") # stop early if there is no date column

    df %>% # start from the input data
        group_by(date) %>% # standardise across stocks within each date
        mutate(across(all_of(Cols), ~(. - mean(., na.rm = T)) / sd(., na.rm = T))) %>% # subtract the cross-sectional mean and divide by the cross-sectional standard deviation
        mutate(across(all_of(Cols), ~pmax(pmin(., Cap), -Cap))) %>% # winsorise at plus and minus Cap standard deviations
        ungroup() # remove the grouping again

}
