# Calculates the weighted average factor score of each portfolio on each month end, giving a composite exposure that can be compared across strategies.
Portfolio_Factor_Scores <- function(Holdings, Scores){ # define Portfolio_Factor_Scores, with inputs Holdings and Scores

    # For stepping through uncomment:
    # Holdings <- All_Holdings ; Scores <- Scores_Long

    if( !all(c("date", "Portfolio", "Tickers", "Weight") %in% names(Holdings)) ) stop("... Holdings must have columns called 'date', 'Portfolio', 'Tickers' and 'Weight'") # stop early if the holdings are not in the expected shape
    if( !all(c("date", "Tickers", "Factor", "Score") %in% names(Scores)) ) stop("... Scores must have columns called 'date', 'Tickers', 'Factor' and 'Score'") # stop early if the scores are not in the expected shape

    Holdings %>% # start from the holdings
        mutate(YM = format(date, "%Y%m")) %>% # match on year and month, since the two sources do not always share the exact month end date
        select(YM, Portfolio, Tickers, Weight) %>% # keep only what the join needs
        inner_join(Scores %>% mutate(YM = format(date, "%Y%m")), by = c("YM", "Tickers"), relationship = "many-to-many") %>% # attach every factor score to every holding, which is many to many by design
        group_by(date, Portfolio, Factor) %>% # one composite score per portfolio, factor and month
        summarise(Score = sum(Weight * Score, na.rm = T) / sum(Weight[!is.na(Score)]), # the weighted average score, rescaled over the holdings that have a score
                  .groups = "drop") # drop the grouping once the summary is done

}
