# Calculates the rolling annualised standard deviation of returns for every series in a tidy data frame.
Roll_SD_Calc <- function(df, NSel = 63, Ann = 252){ # define Roll_SD_Calc, with inputs df, NSel and Ann

    if( !"date" %in% names(df)) stop("... for Roll_SD_Calc to work, provide a date column called 'date'") # stop early if there is no date column
    if( !"Name" %in% names(df)) stop("... for Roll_SD_Calc to work, provide an id column called 'Name'") # stop early if there is no series name column
    if( !"Returns" %in% names(df)) stop("... for Roll_SD_Calc to work, provide a return column called 'Returns'") # stop early if there is no return column

    df %>% # start from the input data
        arrange(date) %>% # order by date so that the window rolls forward in time
        group_by(Name) %>% # roll separately for each series
        mutate(RollSD = RcppRoll::roll_sd(Returns, n = NSel, fill = NA, align = "right") * sqrt(Ann)) %>% # standard deviation over the past NSel observations, scaled to a yearly number
        mutate(Avg_SD = mean(RollSD, na.rm = T)) %>% # the full sample average of the rolling number, used as a reference line
        ungroup() %>% # remove the grouping again
        filter(!is.na(RollSD)) # drop the start of the sample, where the window is not yet full

}
