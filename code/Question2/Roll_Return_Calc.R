# Calculates rolling annualised returns from monthly returns for every series, as in the rolling returns section of Practical 1.
Roll_Return_Calc <- function(df, NSel = 36){ # define Roll_Return_Calc, with inputs df and NSel

    if( !all(c("date", "Name", "Returns") %in% names(df)) ) stop("... df must have columns called 'date', 'Name' and 'Returns'") # stop early if the data is not in the expected shape

    df %>% # start from the input data
        arrange(date) %>% # order by date before rolling
        group_by(Name) %>% # roll within each series
        mutate(RollRet = RcppRoll::roll_prod(1 + Returns, n = NSel, fill = NA, align = "right")^(12 / NSel) - 1) %>% # the compounded return over the window, annualised
        ungroup() %>% # remove the grouping again
        filter(!is.na(RollRet)) # keep only complete windows

}
